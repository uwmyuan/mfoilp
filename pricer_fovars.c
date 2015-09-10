/**@file   pricer_fovars.c
 * @brief  fovars variable pricer
 * @author James Cussens
 */

/*---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2*/

/*#define SCIP_DEBUG*/
#include <assert.h>

#include "pricer_fovars.h"
#include "cfoilp.h"
#include "cons_folinear.h"
#include "scip/scipdefplugins.h"



#define PRICER_NAME            "fovars"
#define PRICER_DESC            "first-order variable pricer"
#define PRICER_PRIORITY        0
#define PRICER_DELAY           TRUE     /* only call pricer if all problem variables have non-negative reduced costs */




/*
 * Data structures
 */

/* TODO: fill in the necessary variable pricer data */

/** variable pricer data */
struct SCIP_PricerData
{
   int dummy;
};




/*
 * Local methods
 */

/* put your local methods here, and declare them static */


static
SCIP_RETCODE pricing(
   SCIP* scip,           /**< SCIP data structure */
   int isfarkas          /**< whether we perform Farkas pricing */
   )
{  

   SCIP_CONS* cons;
   SCIP_ROW* row;
   SCIP_Real dualval;
   SCIP_PROBDATA*  probdata = SCIPgetProbData(scip);	

   int i;

   MR_IntList cons_indices = MR_list_empty();
   MR_FloatList cons_values = MR_list_empty();

   MR_IntList row_indices = MR_list_empty();
   MR_FloatList row_values = MR_list_empty();

   MR_IntList idents;
   MR_StringList names;
   MR_FloatList lbs;
   MR_FloatList ubs;
   MR_IntList vartypes;
   MR_FloatList objs;

   MR_AtomStore new_atom_store;

   int ident;
   MR_String name;
   SCIP_Real lb;
   SCIP_Real ub;
   SCIP_Real obj;
   int vartype;

   SCIP_VAR* var;

   MR_IntList varcons_indices;
   MR_FloatList coeffs;
   int c;
   SCIP_Real coeff;

   assert(probdata != NULL);

   /* Construct dual solution for Mercury */

   /* dual values from linear constraints */

   assert(probdata->conss != NULL);
   assert(probdata->cons_store != NULL);

   for( i = 0; i < probdata->nconss; ++i )
   {
      cons = probdata->conss[i];
      if( isfarkas )
         dualval = SCIPgetDualfarkasLinear(scip,cons);
      else
         dualval = SCIPgetDualsolLinear(scip,cons);
#ifdef SCIP_DEBUG
      SCIPdebugMessage("constraint <%d> has dual value <%g>.\n", i, dualval);
      SCIPdebug(  SCIPprintCons(scip, cons, NULL)  );
#endif

      if( !SCIPisZero(scip, dualval) )
      {
         cons_indices = MR_list_cons( i, cons_indices);
         cons_values = MR_list_cons( MR_float_to_word(dualval), cons_values);
      }

   }

   assert(probdata->rows != NULL);
   assert(probdata->row_store != NULL);

   /* dual values from cutting planes */

   for( i = 0; i < probdata->nrows; ++i )
   {
      row = probdata->rows[i];
      if( row != NULL )
         if( isfarkas )
            dualval = SCIProwGetDualfarkas(row);
         else
            dualval = SCIProwGetDualsol(row);
      else
         dualval = 0.0;
#ifdef SCIP_DEBUG
      SCIPdebugMessage("cutting plane <%d> has dual value <%g>.\n", i, dualval);
      if( row != NULL )
         SCIPdebug( SCIPprintRow(scip, row, NULL) );
#endif
      if( !SCIPisZero(scip, dualval) )
      {
         row_indices = MR_list_cons( i, row_indices);
         row_values = MR_list_cons( MR_float_to_word(dualval), row_values);
      }

   }

   /* Get Mercury to find reduced cost variables */

   MR_delayed_variables(
      probdata->cons_store, /* Mercury mapping from constraint indices to Mercury constraint terms */
      cons_indices,         /* constraint indices for constraints with non-zero dual value */
      cons_values,          /* dual values corresponding to each (indexed) constraint */
      probdata->row_store, /* Mercury mapping from cutting plane indices to Mercury constraint terms */
      row_indices,         /* cutting plane indices for cutting planes with non-zero dual value */
      row_values,          /* dual values corresponding to each (indexed) cutting plane */
      probdata->nvars,     /* current number of variables in the MIP */
      isfarkas,            /* whether we are perfoming farkas pricing */
      &idents,             /* index for each variable to be created */
      &names,              /* name for each variable to be created */
      &lbs,                /* lower bound for each variable to be created */
      &ubs,                /* upper bound for each variable to be created */
      &vartypes,           /* variable type for each variable to be created */
      &objs,                /* objective coeff for each variable to be created */
      probdata->atom_store, /* current store of atoms/variables */
      &new_atom_store       /* new store of atoms/variables */
      );              
   
   
   /* update atom store */

   probdata->atom_store = new_atom_store;

   /* Add new variables */

   SCIPdebugMessage("Pricing in new variables\n");

   while ( !MR_list_is_empty(idents) ) 
   {
      ident =   MR_list_head(idents);
      name =    (MR_String) MR_list_head(names);
      lb =      MR_word_to_float(MR_list_head(lbs));
      ub =      MR_word_to_float(MR_list_head(ubs));
      obj =     MR_word_to_float(MR_list_head(objs));
      vartype = MR_list_head(vartypes);

      assert( ident == probdata->nvars );

      SCIP_CALL( SCIPcreateVarBasic(scip, &var, name, lb, ub, obj, vartype) );
      SCIP_CALL( SCIPaddPricedVar(scip, var, 1.0) );
      
      
      SCIPdebug(SCIPprintVar(scip, var, NULL) );


      if( !(ident < probdata->vars_len) )
      {
         probdata->vars_len += VAR_BLOCKSIZE;
         SCIP_CALL( SCIPreallocMemoryArray(scip, &(probdata->vars), probdata->vars_len) );
      }
      probdata->vars[probdata->nvars++] = var;

      /* 'add' new variable to the single folinear constraint
         ( this just locks the variable ) */
      
      SCIP_CALL( addCoefFolinear(scip, probdata->folinearcons, var, ident, probdata->atom_store) );

      /* add variable to any linear constraints in which it appears 
         with non-zero coefficient */
      
      MR_varcoeffs(probdata->atom_store, ident, probdata->cons_store, &varcons_indices, &coeffs);

      while ( !MR_list_is_empty(varcons_indices) ) 
      {
         c = MR_list_head(varcons_indices);
         coeff = MR_word_to_float(MR_list_head(coeffs));

         SCIP_CALL( SCIPaddCoefLinear(scip, probdata->conss[c], var, coeff) );

          varcons_indices = MR_list_tail(varcons_indices);
          coeffs = MR_list_tail(coeffs);
      }
      

      /* add variable to any cutting planes in which it appears 
         with non-zero coefficient */
      
      MR_varcoeffs(probdata->atom_store, ident, probdata->row_store, &varcons_indices, &coeffs);

      while ( !MR_list_is_empty(varcons_indices) ) 
      {
         c = MR_list_head(varcons_indices);
         coeff = MR_word_to_float(MR_list_head(coeffs));

         SCIP_CALL( SCIPaddVarToRow(scip, probdata->rows[c], var, coeff) );

          varcons_indices = MR_list_tail(varcons_indices);
          coeffs = MR_list_tail(coeffs);
      }
      

      /* Now need to add new variable to existing constraints and cutting planes.
         The former with SCIPaddCoefLinear and the latter with SCIPaddVarToRow */

      idents = MR_list_tail(idents);
      names = MR_list_tail(names);
      lbs = MR_list_tail(lbs);
      ubs = MR_list_tail(ubs);
      objs = MR_list_tail(objs);
      vartypes = MR_list_tail(vartypes);
   }

   SCIPdebugMessage("Pricing complete\n");

   return SCIP_OKAY;
}




/*
 * Callback methods of variable pricer
 */

/* TODO: Implement all necessary variable pricer methods. The methods with an #if 0 ... #else #define ... are optional */

/** copy method for pricer plugins (called when SCIP copies plugins) */
#if 0
static
SCIP_DECL_PRICERCOPY(pricerCopyFovars)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of fovars variable pricer not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define pricerCopyFovars NULL
#endif

/** destructor of variable pricer to free user data (called when SCIP is exiting) */
#if 0
static
SCIP_DECL_PRICERFREE(pricerFreeFovars)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of fovars variable pricer not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define pricerFreeFovars NULL
#endif


/** initialization method of variable pricer (called after problem was transformed) */
#if 0
static
SCIP_DECL_PRICERINIT(pricerInitFovars)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of fovars variable pricer not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define pricerInitFovars NULL
#endif


/** deinitialization method of variable pricer (called before transformed problem is freed) */
#if 0
static
SCIP_DECL_PRICEREXIT(pricerExitFovars)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of fovars variable pricer not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define pricerExitFovars NULL
#endif


/** solving process initialization method of variable pricer (called when branch and bound process is about to begin) */
#if 0
static
SCIP_DECL_PRICERINITSOL(pricerInitsolFovars)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of fovars variable pricer not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define pricerInitsolFovars NULL
#endif


/** solving process deinitialization method of variable pricer (called before branch and bound process data is freed) */
#if 0
static
SCIP_DECL_PRICEREXITSOL(pricerExitsolFovars)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of fovars variable pricer not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define pricerExitsolFovars NULL
#endif


/** reduced cost pricing method of variable pricer for feasible LPs */
static
SCIP_DECL_PRICERREDCOST(pricerRedcostFovars)
{  /*lint --e{715}*/
 
   (*result) = SCIP_DIDNOTRUN;

   SCIPdebugMessage("call scip_redcost ...\n");

   SCIP_CALL( pricing(scip, 0) );

   (*result) = SCIP_SUCCESS;

   return SCIP_OKAY;
}


/** Farkas pricing method of variable pricer for infeasible LPs */
static
SCIP_DECL_PRICERFARKAS(pricerFarkasFovars)
{ 
   SCIPdebugMessage("call scip_farkas ...\n");

   SCIP_CALL( pricing(scip, 1) );

   return SCIP_OKAY;
}





/*
 * variable pricer specific interface methods
 */

/** creates the fovars variable pricer and includes it in SCIP */
SCIP_RETCODE SCIPincludePricerFovars(
   SCIP*                 scip                /**< SCIP data structure */
   )
{
   SCIP_PRICERDATA* pricerdata;
   SCIP_PRICER* pricer;

   /* create fovars variable pricer data */
   pricerdata = NULL;
   /* TODO: (optional) create variable pricer specific data here */

   pricer = NULL;

   /* include variable pricer */

   /* use SCIPincludePricerBasic() plus setter functions if you want to set callbacks one-by-one and your code should
    * compile independent of new callbacks being added in future SCIP versions
    */
   SCIP_CALL( SCIPincludePricerBasic(scip, &pricer, PRICER_NAME, PRICER_DESC, PRICER_PRIORITY, PRICER_DELAY,
         pricerRedcostFovars, pricerFarkasFovars, pricerdata) );
   assert(pricer != NULL);

   /* set non fundamental callbacks via setter functions */
   /* SCIP_CALL( SCIPsetPricerCopy(scip, pricer, pricerCopyFovars) ); */
   /* SCIP_CALL( SCIPsetPricerFree(scip, pricer, pricerFreeFovars) ); */
   /* SCIP_CALL( SCIPsetPricerInit(scip, pricer, pricerInitFovars) ); */
   /* SCIP_CALL( SCIPsetPricerExit(scip, pricer, pricerExitFovars) ); */
   /* SCIP_CALL( SCIPsetPricerInitsol(scip, pricer, pricerInitsolFovars) ); */
   /* SCIP_CALL( SCIPsetPricerExitsol(scip, pricer, pricerExitsolFovars) ); */


   /* add fovars variable pricer parameters */
   /* TODO: (optional) add variable pricer specific parameters with SCIPaddTypeParam() here */

   return SCIP_OKAY;
}
