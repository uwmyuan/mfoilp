/**@file   cons_folinear.c
 * @brief  constraint handler for folinear constraints
 * @author James Cussens
 */

/*---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2*/
/*#define SCIP_DEBUG*/
#include <assert.h>
#include <string.h>

#include <cons_folinear.h>
#include <cfoilp.h>


/* fundamental constraint handler properties */
#define CONSHDLR_NAME          "folinear"
#define CONSHDLR_DESC          "first order linear constraint handler"
#define CONSHDLR_ENFOPRIORITY         10 /**< priority of the constraint handler for constraint enforcing */
#define CONSHDLR_CHECKPRIORITY        0 /**< priority of the constraint handler for checking feasibility */
#define CONSHDLR_EAGERFREQ          100 /**< frequency for using all instead of only the useful constraints in separation,
                                              *   propagation and enforcement, -1 for no eager evaluations, 0 for first only */
#define CONSHDLR_NEEDSCONS         TRUE /**< should the constraint handler be skipped, if no constraints are available? */

/* optional constraint handler properties */
/* TODO: remove properties which are never used because the corresponding routines are not supported */
#define CONSHDLR_SEPAPRIORITY         0 /**< priority of the constraint handler for separation */
#define CONSHDLR_SEPAFREQ            -1 /**< frequency for separating cuts; zero means to separate only in the root node */
#define CONSHDLR_DELAYSEPA        FALSE /**< should separation method be delayed, if other separators found cuts? */

#define CONSHDLR_PROPFREQ            -1 /**< frequency for propagating domains; zero means only preprocessing propagation */
#define CONSHDLR_DELAYPROP        FALSE /**< should propagation method be delayed, if other propagators found reductions? */
#define CONSHDLR_PROP_TIMING       SCIP_PROPTIMING_BEFORELP/**< propagation timing mask of the constraint handler*/

#define CONSHDLR_MAXPREROUNDS        -1 /**< maximal number of presolving rounds the constraint handler participates in (-1: no limit) */
#define CONSHDLR_DELAYPRESOL      FALSE /**< should presolving method be delayed, if other presolvers found reductions? */



/* TODO: (optional) enable linear or nonlinear constraint upgrading */
#if 0
#include "scip/cons_linear.h"
#include "scip/cons_nonlinear.h"
#define LINCONSUPGD_PRIORITY          0 /**< priority of the constraint handler for upgrading of linear constraints */
#define NONLINCONSUPGD_PRIORITY       0 /**< priority of the constraint handler for upgrading of nonlinear constraints */
#endif


/*
 * Data structures
 */

/* TODO: fill in the necessary constraint data */

/** constraint data for folinear constraints */
struct SCIP_ConsData
{
   SCIP_VAR** vars;
   SCIP_Real nvars;
   SCIP_Bool* down;
   SCIP_Bool* up;
};

/** constraint handler data */
/* struct SCIP_ConshdlrData */
/* {  */
/* };  */


/*
 * Local methods
 */

/* put your local methods here, and declare them static */

/* complete this later */

/* SCIP_RETCODE sol2mercury( */
/*    SCIP* scip, */
/*    SCIP_VAR** vars, */
/*    int nvars, */
/*    SCIP_SOL* sol, */
   





/*
 * Linear constraint upgrading
 */

#ifdef LINCONSUPGD_PRIORITY
/** tries to upgrade a linear constraint into a folinear constraint */
static
SCIP_DECL_LINCONSUPGD(linconsUpgdFolinear)
{  /*lint --e{715}*/
   SCIP_Bool upgrade;

   assert(upgdcons != NULL);

   /* check, if linear constraint can be upgraded to folinear constraint */
   upgrade = FALSE;
   /* TODO: put the constraint's properties here, in terms of the statistics given by nposbin, nnegbin, ... */

   if( upgrade )
   {
      SCIPdebugMessage("upgrading constraint <%s> to folinear constraint\n", SCIPconsGetName(cons));

      /* create the bin Folinear constraint (an automatically upgraded constraint is always unmodifiable) */
      assert(!SCIPconsIsModifiable(cons));
      SCIP_CALL( SCIPcreateConsFolinear(scip, upgdcons, SCIPconsGetName(cons), nvars, vars, vals, lhs, rhs,
            SCIPconsIsInitial(cons), SCIPconsIsSeparated(cons), SCIPconsIsEnforced(cons),
            SCIPconsIsChecked(cons), SCIPconsIsPropagated(cons), SCIPconsIsLocal(cons),
            SCIPconsIsModifiable(cons), SCIPconsIsDynamic(cons), SCIPconsIsRemovable(cons),
            SCIPconsIsStickingAtNode(cons)) );
   }

   return SCIP_OKAY;
}
#endif


/*
 * Callback methods of constraint handler
 */

/* TODO: Implement all necessary constraint handler methods. The methods with #if 0 ... #else #define ... are optional */

/** copy method for constraint handler plugins (called when SCIP copies plugins) */
#if 0
static
SCIP_DECL_CONSHDLRCOPY(conshdlrCopyFolinear)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of folinear constraint handler not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define conshdlrCopyFolinear NULL
#endif

/** destructor of constraint handler to free constraint handler data (called when SCIP is exiting) */
#if 0
static
SCIP_DECL_CONSFREE(consFreeFolinear)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of folinear constraint handler not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define consFreeFolinear NULL
#endif


/** initialization method of constraint handler (called after problem was transformed) */
#if 0
static
SCIP_DECL_CONSINIT(consInitFolinear)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of folinear constraint handler not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define consInitFolinear NULL
#endif


/** deinitialization method of constraint handler (called before transformed problem is freed) */
#if 0
static
SCIP_DECL_CONSEXIT(consExitFolinear)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of folinear constraint handler not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define consExitFolinear NULL
#endif


/** presolving initialization method of constraint handler (called when presolving is about to begin) */
#if 0
static
SCIP_DECL_CONSINITPRE(consInitpreFolinear)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of folinear constraint handler not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define consInitpreFolinear NULL
#endif


/** presolving deinitialization method of constraint handler (called after presolving has been finished) */
#if 0
static
SCIP_DECL_CONSEXITPRE(consExitpreFolinear)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of folinear constraint handler not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define consExitpreFolinear NULL
#endif


/** solving process initialization method of constraint handler (called when branch and bound process is about to begin) */
#if 0
static
SCIP_DECL_CONSINITSOL(consInitsolFolinear)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of folinear constraint handler not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define consInitsolFolinear NULL
#endif


/** solving process deinitialization method of constraint handler (called before branch and bound process data is freed) */
#if 0
static
SCIP_DECL_CONSEXITSOL(consExitsolFolinear)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of folinear constraint handler not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define consExitsolFolinear NULL
#endif


/** frees specific constraint data */
static
SCIP_DECL_CONSDELETE(consDeleteFolinear)
{  /*lint --e{715}*/

   int n;

   assert( scip != NULL );
   assert( conshdlr != NULL );
   assert( strcmp(SCIPconshdlrGetName(conshdlr), CONSHDLR_NAME) == 0 );
   assert( cons != NULL );
   assert( consdata != NULL);
   assert( *consdata != NULL);
   assert( (*consdata)->vars != NULL );
   assert( (*consdata)->down != NULL );
   assert( (*consdata)->up != NULL );

   SCIPdebugMessage("deleting first order linear constraint <%s>.\n", SCIPconsGetName(cons));

   n = (*consdata)->nvars;
   SCIPfreeBlockMemoryArray(scip, &((*consdata)->vars), n);
   SCIPfreeBlockMemoryArray(scip, &((*consdata)->down), n);
   SCIPfreeBlockMemoryArray(scip, &((*consdata)->up), n);
   SCIPfreeBlockMemory(scip, consdata);

   return SCIP_OKAY;
}



/** transforms constraint data into data belonging to the transformed problem */
#if 0
static
SCIP_DECL_CONSTRANS(consTransFolinear)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of folinear constraint handler not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define consTransFolinear NULL
#endif


/** LP initialization method of constraint handler (called before the initial LP relaxation at a node is solved) */
#if 0
static
SCIP_DECL_CONSINITLP(consInitlpFolinear)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of folinear constraint handler not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define consInitlpFolinear NULL
#endif


/** separation method of constraint handler for LP solutions */
#if 0
static
SCIP_DECL_CONSSEPALP(consSepalpFolinear)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of folinear constraint handler not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define consSepalpFolinear NULL
#endif


/** separation method of constraint handler for arbitrary primal solutions */
#if 0
static
SCIP_DECL_CONSSEPASOL(consSepasolFolinear)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of folinear constraint handler not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define consSepasolFolinear NULL
#endif


/** constraint enforcing method of constraint handler for LP solutions */
static
SCIP_DECL_CONSENFOLP(consEnfolpFolinear)
{  /*lint --e{715}*/

   int c;
   int nGen = 0;

   SCIP_CONS* cons;
   SCIP_VAR* var; 
   SCIP_Real val;
   int i;

   MR_IntList indices;
   MR_FloatList values;

   SCIP_CONSDATA* consdata;
   SCIP_PROBDATA* probdata;

   assert( scip != NULL );
   assert( conshdlr != NULL );
   assert( strcmp(SCIPconshdlrGetName(conshdlr), CONSHDLR_NAME) == 0 );
   assert( conss != NULL );
   assert( result != NULL );

   probdata = SCIPgetProbData(scip);
   assert( probdata != NULL );

   /* loop through all constraints */
   for (c = 0; c < nconss; ++c)
   {
      cons = conss[c];
      assert( cons != NULL );
      SCIPdebugMessage("enforcing lp solution for first order linear constraint <%s>.\n", SCIPconsGetName(cons));

      consdata = SCIPconsGetData(conss[c]);

      assert( consdata != NULL );
      assert( consdata->vars != NULL );

      /* get solution values for the variables involved in this constraint */

      indices = MR_list_empty();
      values = MR_list_empty();

      for( i = 0; i < consdata->nvars; ++i )
      {
         var = consdata->vars[i];
         val = SCIPgetSolVal(scip, NULL, var);
         if( !SCIPisZero(scip, val))
         {
            SCIP_CALL( SCIPprintVar(scip, var, NULL) );
            printf("%f\n", val);
            indices = MR_list_cons( i, indices);
            values = MR_list_cons( MR_float_to_word(val), values);
         }
      }

      /* ask Mercury whether there exists a ground instance of this constraint
         which does not satisfy the solution 
      */

      if( MR_existscut((MR_String) SCIPconsGetName(cons),probdata->atom_store,indices,values) )
      {
         printf("Found a cut for %s\n", (MR_String) SCIPconsGetName(cons));
         *result = SCIP_INFEASIBLE;
         return SCIP_OKAY;
      }


      /* all this stuff TODO */

/*       /\* get cuts (if any ) from Mercury *\/ */

/*       MR_cuts(probdata->atom_store,indices,values,probdata->row_store,probdata->nrows,&new_row_store,&row_idents,&names,&lbs,&finlbs,&coeffss,&varss,&ubs,&finubs); */
      
/*       /\* update row store (only for later passing back to Mercury ) *\/ */

/*       probdata->row_store = new_row_store; */

/*       /\* add any cuts to SCIP *\/ */

/*       while ( !MR_list_is_empty(lbs) ) */
/*       { */
/*          MR_FloatList coeffs; */
/*          MR_IntList vars; */

/*          MR_String name; */
/*          SCIP_Real lb; */
/*          int finlb; */
/*          SCIP_Real ub; */
/*          int finub;    */

/*          SCIP_ROW *row; */
/*          SCIP_Bool infeasible; */
/*          char s[SCIP_MAXSTRLEN]; */

/*          coeffs = MR_list_head(coeffss); */
/*          vars = MR_list_head(varss); */
/*          name = (MR_String)  MR_list_head(names); */

/*          finlb = MR_list_head(finlbs); */
/*          if( finlb ) */
/*             lb = MR_word_to_float(MR_list_head(lbs)); */
/*          else */
/*             lb = -SCIPinfinity(scip); */

/*          finub = MR_list_head(finubs); */
/*          if( finub ) */
/*             ub = MR_word_to_float(MR_list_head(ubs)); */
/*          else */
/*             ub = SCIPinfinity(scip); */

/*          row_ident = MR_list_head(row_idents); */
         
/*          /\* throw error if idents are not listed as 0,1,...n *\/ */
/*          if( row_ident != probdata->nrows ) */
/*          { */
/*             SCIPerrorMessage("Mercury did not return list of cutting plane indices  correctly.\n"); */
/*             exit(1); */
/*          }       */
/*          /\* make the cut *\/ */
      
/*          (void) SCIPsnprintf(s, SCIP_MAXSTRLEN, "%s", name); */

/*          /\* note row is set to modifiable to allow priced-in variables to enter *\/ */
/*          SCIP_CALL( SCIPcreateEmptyRowCons(scip, &row, conshdlr, s, lb, ub, FALSE, TRUE, TRUE) ); */
/*          SCIP_CALL( SCIPcacheRowExtensions(scip, row) ); */

/*          while ( !MR_list_is_empty(coeffs) ) */
/*          { */
/*             SCIP_Real coeff; */
            
/*             coeff = MR_word_to_float(MR_list_head(coeffs)); */
/*             var = probdata->vars[MR_list_head(vars)]; */
/*             SCIP_CALL( SCIPaddVarToRow(scip, row, var, coeff) ); */
/*             coeffs = MR_list_tail(coeffs); */
/*             vars = MR_list_tail(vars); */
/*          } */
         
/*          SCIP_CALL( SCIPflushRowExtensions(scip, row) ); */
/* #ifdef SCIP_DEBUG */
/*          SCIPdebug( SCIPprintRow(scip, row, NULL) ); */
/* #endif */
/*          SCIP_CALL( SCIPaddCut(scip, NULL, row, FALSE, &infeasible) ); */
/*          /\*SCIP_CALL( SCIPreleaseRow(scip, &row));*\/ */
/*          ++nGen; */

/*          /\* add row to probdata *\/ */
/*          if( !(row_ident < probdata->rows_len) ) */
/*          { */
/*             probdata->rows_len += VAR_BLOCKSIZE; */
/*             SCIP_CALL( SCIPreallocMemoryArray(scip, &(probdata->rows), probdata->rows_len) ); */
/*          } */
/*          probdata->rows[probdata->nrows++] = row; */
         
/*          if ( infeasible ) */
/*          { */
/*             /\* do not declare that the current subproblem is infeasible */
/*                since pricing may allow us to recover feasibility * */
/*                *result = SCIP_CUTOFF; *\/ */
/*             *result = SCIP_INFEASIBLE; */
/*             return SCIP_OKAY; */
/*          } */

/*          coeffss = MR_list_tail(coeffss); */
/*          varss = MR_list_tail(varss); */
/*          names = MR_list_tail(names); */
/*          lbs = MR_list_tail(lbs); */
/*          finlbs = MR_list_tail(finlbs); */
/*          ubs = MR_list_tail(ubs); */
/*          finubs = MR_list_tail(finubs); */
/*          row_idents = MR_list_tail(row_idents); */

/*       } */
/*       /\* return as soon as we find a constraint which generated some cuts *\/ */
/*       if (nGen > 0) */
/*       { */
/* 	 *result = SCIP_SEPARATED; */
/* 	 return SCIP_OKAY; */
/*       } */
   }
   SCIPdebugMessage("all first-order linear constraints are feasible.\n");
   *result = SCIP_FEASIBLE;
   return SCIP_OKAY;
}

/** constraint enforcing method of constraint handler for pseudo solutions */
static
SCIP_DECL_CONSENFOPS(consEnfopsFolinear)
{  /*lint --e{715}*/

   int c;

   SCIP_CONSDATA* consdata;
   SCIP_PROBDATA* probdata;

   SCIP_CONS* cons;
   SCIP_VAR* var; 
   SCIP_Real val;
   int i;

   MR_IntList indices;
   MR_FloatList values;

   assert( scip != NULL );
   assert( conshdlr != NULL );
   assert( strcmp(SCIPconshdlrGetName(conshdlr), CONSHDLR_NAME) == 0 );
   assert( conss != NULL );
   assert( result != NULL );

   probdata = SCIPgetProbData(scip);
   assert( probdata != NULL );

   /* loop through all constraints */
   for (c = 0; c < nconss; ++c)
   {
      cons = conss[c];
      assert( cons != NULL );
      SCIPdebugMessage("enforcing pseudo solution for first order linear constraint <%s>.\n", SCIPconsGetName(cons));
    
      consdata = SCIPconsGetData(conss[c]);

      assert( consdata != NULL );
      assert( consdata->vars != NULL );

      /* get solution values for the variables involved in this constraint */

      indices = MR_list_empty();
      values = MR_list_empty();

      for( i = 0; i < consdata->nvars; ++i )
      {
         var = consdata->vars[i];
         val = SCIPgetSolVal(scip, NULL, var);
         if( !SCIPisZero(scip, val))
         {
            indices = MR_list_cons( i, indices);
            values = MR_list_cons( MR_float_to_word(val), values);
         }
      }

      /* ask Mercury whether there exists a ground instance of this constraint
         which does not satisfy the solution 
      */

      if( MR_existscut((MR_String) SCIPconsGetName(cons),probdata->atom_store,indices,values) )
      {
         SCIPdebugMessage("constraint <%s> infeasible.\n", SCIPconsGetName(cons));
         *result = SCIP_INFEASIBLE;
         return SCIP_OKAY;
      }

   }
   SCIPdebugMessage("all first-order linear constraints are feasible.\n");
   *result = SCIP_FEASIBLE;
   return SCIP_OKAY;

}


/** feasibility check method of constraint handler for integral solutions */
static
SCIP_DECL_CONSCHECK(consCheckFolinear)
{  /*lint --e{715}*/

   int c;

   SCIP_CONS* cons;
   SCIP_VAR* var; 
   SCIP_Real val;
   int i;

   SCIP_CONSDATA* consdata;
   SCIP_PROBDATA* probdata;
   
   MR_IntList indices;
   MR_FloatList values;

   assert( scip != NULL );
   assert( conshdlr != NULL );
   assert( strcmp(SCIPconshdlrGetName(conshdlr), CONSHDLR_NAME) == 0 );
   assert( conss != NULL );
   assert( result != NULL );

   probdata = SCIPgetProbData(scip);
   assert( probdata != NULL );
   
   /* loop through all constraints */
   for (c = 0; c < nconss; ++c)
   {
      cons = conss[c];
      assert( cons != NULL );
      SCIPdebugMessage("checking first order linear constraint <%s>.\n", SCIPconsGetName(cons));

      consdata = SCIPconsGetData(conss[c]);

      assert( consdata != NULL );
      assert( consdata->vars != NULL );

      /* get solution values for the variables involved in this constraint */

      indices = MR_list_empty();
      values = MR_list_empty();

      for( i = 0; i < consdata->nvars; ++i )
      {
         var = consdata->vars[i];
         val = SCIPgetSolVal(scip, sol, var);
         if( !SCIPisZero(scip, val))
         {
            indices = MR_list_cons( i, indices);
            values = MR_list_cons( MR_float_to_word(val), values);
         }
      }

      /* ask Mercury whether there exists a ground instance of this constraint
         which does not satisfy the solution 
      */

      if( MR_existscut((MR_String) SCIPconsGetName(cons),probdata->atom_store,indices,values) )
      {
         *result = SCIP_INFEASIBLE;
         return SCIP_OKAY;
      }
   }
   
   *result = SCIP_FEASIBLE;
   return SCIP_OKAY;
}


/** domain propagation method of constraint handler */
#if 0
static
SCIP_DECL_CONSPROP(consPropFolinear)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of folinear constraint handler not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define consPropFolinear NULL
#endif


/** presolving method of constraint handler */
#if 0
static
SCIP_DECL_CONSPRESOL(consPresolFolinear)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of folinear constraint handler not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define consPresolFolinear NULL
#endif


/** propagation conflict resolving method of constraint handler */
#if 0
static
SCIP_DECL_CONSRESPROP(consRespropFolinear)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of folinear constraint handler not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define consRespropFolinear NULL
#endif


/** variable rounding lock method of constraint handler */
static
SCIP_DECL_CONSLOCK(consLockFolinear)
{  /*lint --e{715}*/

   SCIP_CONSDATA* consdata;

   int i;
   SCIP_VAR* var;


   assert( scip != NULL );
   assert( conshdlr != NULL );
   assert( strcmp(SCIPconshdlrGetName(conshdlr), CONSHDLR_NAME) == 0 );
   assert( cons != NULL );

   SCIPdebugMessage("locking first order linear constraint <%s>.\n", SCIPconsGetName(cons));

   consdata = SCIPconsGetData(cons);

   assert( consdata != NULL );
   assert( consdata->down != NULL );
   assert( consdata->up != NULL );
   assert( consdata->vars != NULL );

   for( i = 0; i < consdata->nvars; ++i )
   {
      var = consdata->vars[i];

      if( consdata->up[i] )
      {
         
         SCIPdebugMessage("adding up lock for variable <%s>\n", SCIPvarGetName(var));
         
         if( consdata->down[i] )
         {
            SCIPdebugMessage("adding down lock for variable <%s>\n", SCIPvarGetName(var));            
            SCIPaddVarLocks(scip, var, nlockspos + nlocksneg, nlockspos + nlocksneg);
         }
         else
            SCIPaddVarLocks(scip, var, nlocksneg, nlockspos);
      }
      else if( consdata->down[i] )
      {
         SCIPaddVarLocks(scip, var, nlockspos, nlocksneg);
         SCIPdebugMessage("adding down lock for variable <%s>\n", SCIPvarGetName(var));            
      }

   }
   
   return SCIP_OKAY;
}


/** constraint activation notification method of constraint handler */
#if 0
static
SCIP_DECL_CONSACTIVE(consActiveFolinear)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of folinear constraint handler not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define consActiveFolinear NULL
#endif


/** constraint deactivation notification method of constraint handler */
#if 0
static
SCIP_DECL_CONSDEACTIVE(consDeactiveFolinear)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of folinear constraint handler not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define consDeactiveFolinear NULL
#endif


/** constraint enabling notification method of constraint handler */
#if 0
static
SCIP_DECL_CONSENABLE(consEnableFolinear)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of folinear constraint handler not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define consEnableFolinear NULL
#endif


/** constraint disabling notification method of constraint handler */
#if 0
static
SCIP_DECL_CONSDISABLE(consDisableFolinear)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of folinear constraint handler not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define consDisableFolinear NULL
#endif

/** variable deletion of constraint handler */
#if 0
static
SCIP_DECL_CONSDELVARS(consDelvarsFolinear)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of folinear constraint handler not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define consDelvarsFolinear NULL
#endif


/** constraint display method of constraint handler */
#if 0
static
SCIP_DECL_CONSPRINT(consPrintFolinear)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of folinear constraint handler not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define consPrintFolinear NULL
#endif


/** constraint copying method of constraint handler */
#if 0
static
SCIP_DECL_CONSCOPY(consCopyFolinear)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of folinear constraint handler not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define consCopyFolinear NULL
#endif


/** constraint parsing method of constraint handler */
#if 0
static
SCIP_DECL_CONSPARSE(consParseFolinear)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of folinear constraint handler not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define consParseFolinear NULL
#endif


/** constraint method of constraint handler which returns the variables (if possible) */
#if 0
static
SCIP_DECL_CONSGETVARS(consGetVarsFolinear)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of folinear power constraint handler not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define consGetVarsFolinear NULL
#endif

/** constraint method of constraint handler which returns the number of variables (if possible) */
#if 0
static
SCIP_DECL_CONSGETNVARS(consGetNVarsFolinear)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of folinear power constraint handler not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define consGetNVarsFolinear NULL
#endif


/*
 * constraint specific interface methods
 */

/** creates the handler for folinear constraints and includes it in SCIP */
SCIP_RETCODE SCIPincludeConshdlrFolinear(
   SCIP*                 scip                /**< SCIP data structure */
   )
{

   SCIP_CONSHDLR* conshdlr;

   conshdlr = NULL;

   /* include constraint handler */

   /* use SCIPincludeConshdlrBasic() plus setter functions if you want to set callbacks one-by-one and your code should
    * compile independent of new callbacks being added in future SCIP versions
    */
   SCIP_CALL( SCIPincludeConshdlrBasic(scip, &conshdlr, CONSHDLR_NAME, CONSHDLR_DESC,
         CONSHDLR_ENFOPRIORITY, CONSHDLR_CHECKPRIORITY, CONSHDLR_EAGERFREQ, CONSHDLR_NEEDSCONS,
         consEnfolpFolinear, consEnfopsFolinear, consCheckFolinear, consLockFolinear,
         NULL) );
   assert(conshdlr != NULL);

   /* set non-fundamental callbacks via specific setter functions */
   /* SCIP_CALL( SCIPsetConshdlrActive(scip, conshdlr, consActiveFolinear) ); */
   /* SCIP_CALL( SCIPsetConshdlrCopy(scip, conshdlr, conshdlrCopyFolinear, consCopyFolinear) ); */
   /* SCIP_CALL( SCIPsetConshdlrDeactive(scip, conshdlr, consDeactiveFolinear) ); */
   SCIP_CALL( SCIPsetConshdlrDelete(scip, conshdlr, consDeleteFolinear) );
   /* SCIP_CALL( SCIPsetConshdlrDelvars(scip, conshdlr, consDelvarsFolinear) ); */
   /* SCIP_CALL( SCIPsetConshdlrDisable(scip, conshdlr, consDisableFolinear) ); */
   /* SCIP_CALL( SCIPsetConshdlrEnable(scip, conshdlr, consEnableFolinear) ); */
   /* SCIP_CALL( SCIPsetConshdlrExit(scip, conshdlr, consExitFolinear) ); */
   /* SCIP_CALL( SCIPsetConshdlrExitpre(scip, conshdlr, consExitpreFolinear) ); */
   /* SCIP_CALL( SCIPsetConshdlrExitsol(scip, conshdlr, consExitsolFolinear) ); */
   /* SCIP_CALL( SCIPsetConshdlrFree(scip, conshdlr, consFreeFolinear) ); */
   /* SCIP_CALL( SCIPsetConshdlrGetVars(scip, conshdlr, consGetVarsFolinear) ); */
   /* SCIP_CALL( SCIPsetConshdlrGetNVars(scip, conshdlr, consGetNVarsFolinear) ); */
   /* SCIP_CALL( SCIPsetConshdlrInit(scip, conshdlr, consInitFolinear) ); */
   /* SCIP_CALL( SCIPsetConshdlrInitpre(scip, conshdlr, consInitpreFolinear) ); */
   /* SCIP_CALL( SCIPsetConshdlrInitsol(scip, conshdlr, consInitsolFolinear) ); */
   /* SCIP_CALL( SCIPsetConshdlrInitlp(scip, conshdlr, consInitlpFolinear) ); */
   /* SCIP_CALL( SCIPsetConshdlrParse(scip, conshdlr, consParseFolinear) ); */
   /* SCIP_CALL( SCIPsetConshdlrPresol(scip, conshdlr, consPresolFolinear, CONSHDLR_MAXPREROUNDS, CONSHDLR_DELAYPRESOL) ); */
   /* SCIP_CALL( SCIPsetConshdlrPrint(scip, conshdlr, consPrintFolinear) ); */
   /* SCIP_CALL( SCIPsetConshdlrProp(scip, conshdlr, consPropFolinear, CONSHDLR_PROPFREQ, CONSHDLR_DELAYPROP, */
   /*       CONSHDLR_PROP_TIMING) ); */
   /* SCIP_CALL( SCIPsetConshdlrResprop(scip, conshdlr, consRespropFolinear) ); */
   /* SCIP_CALL( SCIPsetConshdlrSepa(scip, conshdlr, consSepalpFolinear, consSepasolFolinear, CONSHDLR_SEPAFREQ, CONSHDLR_SEPAPRIORITY, CONSHDLR_DELAYSEPA) ); */
   /* SCIP_CALL( SCIPsetConshdlrTrans(scip, conshdlr, consTransFolinear) ); */

   /* add folinear constraint handler parameters */
   /* TODO: (optional) add constraint handler specific parameters with SCIPaddTypeParam() here */

   return SCIP_OKAY;
}

/** creates and captures a folinear constraint
 *
 *  @note the constraint gets captured, hence at one point you have to release it using the method SCIPreleaseCons()
 */
SCIP_RETCODE SCIPcreateConsFolinear(
   SCIP*                 scip,               /**< SCIP data structure */
   SCIP_CONS**           cons,               /**< pointer to hold the created constraint */
   const char*           name,               /**< name of constraint */
   SCIP_VAR**            vars,               /**< vars in the constraint */
   int                   nvars,              /**< number of vars in the constraint */
   SCIP_Bool*            down,               /**< whether a variable is down locked */
   SCIP_Bool*            up,                 /**< whether a variable is up locked */
   SCIP_Bool             initial,            /**< should the LP relaxation of constraint be in the initial LP?
                                              *   Usually set to TRUE. Set to FALSE for 'lazy constraints'. */
   SCIP_Bool             separate,           /**< should the constraint be separated during LP processing?
                                              *   Usually set to TRUE. */
   SCIP_Bool             enforce,            /**< should the constraint be enforced during node processing?
                                              *   TRUE for model constraints, FALSE for additional, redundant constraints. */
   SCIP_Bool             check,              /**< should the constraint be checked for feasibility?
                                              *   TRUE for model constraints, FALSE for additional, redundant constraints. */
   SCIP_Bool             propagate,          /**< should the constraint be propagated during node processing?
                                              *   Usually set to TRUE. */
   SCIP_Bool             local,              /**< is constraint only valid locally?
                                              *   Usually set to FALSE. Has to be set to TRUE, e.g., for branching constraints. */
   SCIP_Bool             modifiable,         /**< is constraint modifiable (subject to column generation)?
                                              *   Usually set to FALSE. In column generation applications, set to TRUE if pricing
                                              *   adds coefficients to this constraint. */
   SCIP_Bool             dynamic,            /**< is constraint subject to aging?
                                              *   Usually set to FALSE. Set to TRUE for own cuts which
                                              *   are separated as constraints. */
   SCIP_Bool             removable,          /**< should the relaxation be removed from the LP due to aging or cleanup?
                                              *   Usually set to FALSE. Set to TRUE for 'lazy constraints' and 'user cuts'. */
   SCIP_Bool             stickingatnode      /**< should the constraint always be kept at the node where it was added, even
                                              *   if it may be moved to a more global node?
                                              *   Usually set to FALSE. Set to TRUE to for constraints that represent node data. */
   )
{

   SCIP_CONSHDLR* conshdlr;
   SCIP_CONSDATA* consdata;

   int i;

   /* find the folinear constraint handler */
   conshdlr = SCIPfindConshdlr(scip, CONSHDLR_NAME);
   if( conshdlr == NULL )
   {
      SCIPerrorMessage("folinear constraint handler not found\n");
      return SCIP_PLUGINNOTFOUND;
   }

   /* initialise constraint data */
   
   SCIP_CALL( SCIPallocBlockMemory(scip, &consdata) );

   SCIP_CALL( SCIPallocBlockMemoryArray(scip, &consdata->vars, nvars) );
   SCIP_CALL( SCIPallocBlockMemoryArray(scip, &consdata->up, nvars) );
   SCIP_CALL( SCIPallocBlockMemoryArray(scip, &consdata->down, nvars) );

   consdata->nvars = nvars;
   for( i = 0; i < nvars; ++i )
   {
      consdata->vars[i] = vars[i];
      consdata->down[i] = down[i];
      consdata->up[i] = up[i];
   }

   /* create constraint */
   SCIP_CALL( SCIPcreateCons(scip, cons, name, conshdlr, consdata, initial, separate, enforce, check, propagate,
         local, modifiable, dynamic, removable, stickingatnode) );

   return SCIP_OKAY;
}

/** creates and captures a folinear constraint with all its constraint flags set to their
 *  default values
 *
 *  @note the constraint gets captured, hence at one point you have to release it using the method SCIPreleaseCons()
 */
SCIP_RETCODE SCIPcreateConsBasicFolinear(
   SCIP*                 scip,               /**< SCIP data structure */
   SCIP_CONS**           cons,               /**< pointer to hold the created constraint */
   const char*           name,               /**< name of constraint */
   SCIP_VAR**            vars,               /**< vars in the constraint */
   int                   nvars,              /**< number of vars in the constraint */
   SCIP_Bool*            down,               /**< whether a variable is down locked */
   SCIP_Bool*            up                  /**< whether a variable is up locked */
   )
{


   SCIP_CALL( SCIPcreateConsFolinear(scip, cons, name, vars, nvars, down, up,
         TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE) );

   return SCIP_OKAY;
}
