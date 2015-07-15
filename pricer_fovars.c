/**@file   pricer_fovars.c
 * @brief  fovars variable pricer
 * @author James Cussens
 */

/*---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2*/

#include <assert.h>

#include "scip/pricer_fovars.h"


#define PRICER_NAME            "fovars"
#define PRICER_DESC            "variable pricer template"
#define PRICER_PRIORITY        0
#define PRICER_DELAY           TRUE     /* only call pricer if all problem variables have non-negative reduced costs */




/*
 * Data structures
 */

/* TODO: fill in the necessary variable pricer data */

/** variable pricer data */
struct SCIP_PricerData
{
};




/*
 * Local methods
 */

/* put your local methods here, and declare them static */




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

   /* ask Mercury to find variables with reduced cost */

   (*result) = SCIP_DIDNOTRUN;


   MR_delayed_variables(
      atomstore,
      consstore,
      dualsol,
      &idents,             /* index for each variable to be created */
      &names,              /* name for each variable to be created */
      &lbs,                /* lower bound for each variable to be created */
      &ubs,                /* upper bound for each variable to be created */
      &vartypes,           /* variable type for each variable to be created */
      &objs);              /* objective coeff for each variable to be created */
   


   while ( !MR_list_is_empty(idents) ) 
   {
      ident =   MR_list_head(idents);
      name =    (MR_String) MR_list_head(names);
      lb =      MR_word_to_float(MR_list_head(lbs));
      ub =      MR_word_to_float(MR_list_head(ubs));
      obj =     MR_word_to_float(MR_list_head(objs));
      vartype = MR_list_head(vartypes);

      SCIP_CALL( SCIPcreateVarBasic(scip, &var, name, lb, ub, obj, vartype) );
      SCIP_CALL( SCIPaddPricedVar(scip, var, 1.0) );

      if( !(ident < probdata->vars_len) )
      {
         probdata->vars_len += VAR_BLOCKSIZE;
         SCIP_CALL( SCIPreallocMemoryArray(scip, &(probdata->vars), probdata->vars_len) );
      }
      probdata->vars[probdata->nvars++] = var;

      idents = MR_list_tail(idents);
      names = MR_list_tail(names);
      lbs = MR_list_tail(lbs);
      ubs = MR_list_tail(ubs);
      objs = MR_list_tail(objs);
      vartypes = MR_list_tail(vartypes);
   }

   (*result) = SCIP_SUCCESS;

   return SCIP_OKAY;
}


#if 0
/** Farkas pricing method of variable pricer for infeasible LPs */
static
SCIP_DECL_PRICERFARKAS(pricerFarkasFovars)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of fovars variable pricer not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define pricerFarkasFovars NULL
#endif




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
