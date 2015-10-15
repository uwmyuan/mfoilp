#include <string.h>
#include <stdio.h>
#include <stdlib.h>


#include <scip/scip.h>
#include <scip/scipdefplugins.h>

#include "cons_folinear.h"
#include "pricer_dummy.h"
#include "cfoilp.h"


/*
** This header file is part of the stand-alone interface.
** It contains declarations for mercury_init() and mercury_terminate(),
** which are used to respectively start and shutdown the Mercury runtime.
*/
#include "mfoilp_int.h"

#include "mercury_stuff.h"


/** delete problem data */
static
SCIP_DECL_PROBDELORIG(probdelorigFOILP)
{  /*lint --e{831} */

   assert( probdata != NULL );
   assert( *probdata != NULL );

   SCIPfreeMemoryArray(scip, &(*probdata)->vars);

   /* free probdata */
   SCIPfreeMemory(scip, probdata);

   return SCIP_OKAY;
}

SCIP_RETCODE makeclause(
   SCIP* scip,                /**< SCIP pointer */
   SCIP_PROBDATA*  probdata,  /**< problem data */
   MR_IntList neglits,        /**< indices for negative literals */
   MR_IntList poslits,        /**< indices for positive literals */
   int* nvars,                /**< pointer to number of literals in the clause */
   SCIP_VAR** clausevars,     /**< temporary storage for SCIP variables in clause */
   int* once_only             /**< if either lit 0 is positive and marked as occuring only in 
                                   this clause then = 0; else if lit 1 is positive and so marked = 1; else = -1 */
   )
{
   SCIP_VAR* var;
   SCIP_VAR* negvar;
   int varindex;
   
   (*nvars) = 0;
   (*once_only) = -1;

   while ( !MR_list_is_empty(neglits) )
   {
      varindex = (int) MR_list_head(neglits);
      var = probdata->vars[varindex];
      SCIP_CALL( SCIPgetNegatedVar(scip,var,&negvar) );
      clausevars[(*nvars)++] = negvar;
#ifdef SCIP_DEBUG
      SCIPdebugMessage("Variable in cut:\n");
      SCIPdebug( SCIPprintVar(scip, negvar, NULL) );
#endif

      neglits =  MR_list_tail(neglits);
   }

   while ( !MR_list_is_empty(poslits) )
   {
      varindex = (int) MR_list_head(poslits);
      
      if( (*once_only) == -1 && (*nvars) < 2 && MR_once_only(probdata->atom_store,varindex) )
         *once_only = *nvars;

      var = probdata->vars[varindex];
      clausevars[(*nvars)++] = var;
#ifdef SCIP_DEBUG
      SCIPdebugMessage("Variable in cut:\n");
      SCIPdebug( SCIPprintVar(scip, var, NULL) );
#endif
      
      poslits =  MR_list_tail(poslits);
   }
   return SCIP_OKAY;
}

SCIP_RETCODE addNewVars(
   SCIP*           scip,               /**< SCIP pointer */
   SCIP_PROBDATA*  probdata,           /**< problem data */
   MR_FloatList    objectives,         /**< objectives values for new variables */
   MR_StringList   varnames,           /**< names for new variables */
   SCIP_Bool       initial             /**< whether new variables should be 'initial' */
   )
{

   SCIP_VAR* var;

   /* create binary variables in constraints using "objectives" list */

   while ( !MR_list_is_empty(objectives) ) 
   {
      SCIP_CALL( SCIPcreateVar(scip, &var, 
            (char *) MR_list_head(varnames), 
            0.0, 1.0, 
            (SCIP_Real) MR_word_to_float(MR_list_head(objectives)), 
            SCIP_VARTYPE_BINARY, initial, FALSE, NULL, NULL, NULL, NULL, NULL) );
      SCIP_CALL( SCIPaddVar(scip, var) );

#ifdef SCIP_DEBUG
      SCIPdebugMessage("New variable:\n");
      SCIPdebug( SCIPprintVar(scip, var, NULL) );
#endif

      /* increase size of probdata->vars if necessary */
      if( !(probdata->nvars < probdata->vars_len) )
      {
         probdata->vars_len += VAR_BLOCKSIZE;
         SCIP_CALL( SCIPreallocMemoryArray(scip, &(probdata->vars), probdata->vars_len) );
      }

      /* record variable in array */
      /* value of probdata->nvars will correspond with that of Mercury's atomstore */
      probdata->vars[probdata->nvars++] = var;

      objectives = MR_list_tail(objectives);
      varnames = MR_list_tail(varnames);
   }
   return SCIP_OKAY;
}

/** main function */
int main(
   int                        argc,          /**< number of arguments from the shell */
   char**                     argv           /**< array of shell arguments */
   )
{
   void *stack_bottom;

   SCIP* scip = NULL;
   SCIP_PROBDATA*  probdata = NULL;

   MR_AtomStore atomstore;
   MR_FloatList objectives;
   MR_StringList varnames;
   MR_StringList consnames;
   MR_IntListList neglitss;
   MR_IntListList poslitss;
   MR_IntList neglits;
   MR_IntList poslits;

   SCIP_CONS* cons;

   MR_StringList clausenames;
   SCIP_VAR* clausevars[100];

   const char paramfile[] = "mfoilp.set";

   int nvars;
   int once_only;
   
   
   SCIP_Real vals[2] = {1.0, 1.0};
   int i;
   SCIP_VAR* var;

   /* initialise Mercury runtime */
   mercury_init(argc, argv, &stack_bottom);

   /* initialise SCIP */
   SCIP_CALL( SCIPcreate(&scip) );

   /* include default SCIP plugins */
   SCIP_CALL( SCIPincludeDefaultPlugins(scip) );

   /* include dummy pricer  */
   SCIP_CALL( SCIPincludePricerDummy(scip) );

   /* include first-order linear constraint handler */
   SCIP_CALL( SCIPincludeConshdlrFolinear(scip) );

   /* read in parameters */
   if( SCIPfileExists(paramfile) )
   {
      SCIPverbMessage(scip, SCIP_VERBLEVEL_NORMAL, NULL, "Reading parameter file <%s>.\n", paramfile);
      SCIP_CALL( SCIPreadParams(scip, paramfile) );
   }
   else
   {
      SCIPwarningMessage(scip, "Parameter file <%s> not found - using default settings.\n", paramfile);
   }

   /* allocate memory for probdata */
   SCIP_CALL( SCIPallocMemory(scip, &probdata) );

   /* initialise probdata */
   probdata->nvars = 0;
   probdata->vars = NULL;
   probdata->vars_len = VAR_BLOCKSIZE;
   SCIP_CALL( SCIPallocMemoryArray(scip, &(probdata->vars), probdata->vars_len) );

   /* create problem */
   SCIP_CALL( SCIPcreateProb(scip, "mfoilp", probdelorigFOILP, NULL, NULL,
         NULL, NULL, NULL, probdata) );

   /* activates dummy pricer  */
   SCIP_CALL( SCIPactivatePricer(scip, SCIPfindPricer(scip, "dummy")) );

   /* get initial constraints and variables from Mercury */
   MR_initial_constraints(&atomstore,&objectives,&varnames,&consnames,&neglitss,&poslitss);

   /* initialise SCIP's atom store */
   probdata->atom_store = atomstore;

   /* create initial binary variables in constraints  */
   SCIP_CALL( addNewVars(scip, probdata, objectives, varnames, TRUE) );

   /* now add the initial constraints */
   while ( !MR_list_is_empty(consnames) )
   {
      neglits =  MR_list_head(neglitss);
      poslits =  MR_list_head(poslitss);

      SCIP_CALL( makeclause(scip, probdata, neglits, poslits, &nvars, clausevars, &once_only) );

      if( once_only == -1 )
      {
         /* add a ground clause */
         SCIP_CALL( SCIPcreateConsBasicLogicor(scip, &cons, 
               (char *)  MR_list_head(consnames), 
               nvars, clausevars) );
         SCIP_CALL( SCIPaddCons(scip, cons) );
         /*SCIP_CALL( SCIPprintCons(scip, cons, NULL)  );*/
         SCIP_CALL( SCIPreleaseCons(scip, &cons) );
      }
      else
      {
         /* add an equation */
         SCIP_CALL( SCIPcreateConsBasicLinear(scip, &cons,
               (char *)  MR_list_head(consnames), 2, clausevars, vals, 1.0, 1.0) );
         SCIP_CALL( SCIPaddCons(scip, cons) );
         /*SCIP_CALL( SCIPprintCons(scip, cons, NULL)  );*/
         SCIP_CALL( SCIPreleaseCons(scip, &cons) );

         /* encourage the 'right' one to be aggregated */
         if( once_only == 0 )
            SCIP_CALL( SCIPmarkDoNotMultaggrVar(scip, clausevars[1]) );
         else
            SCIP_CALL( SCIPmarkDoNotMultaggrVar(scip, clausevars[0]) );
      }
      consnames = MR_list_tail(consnames);
      neglitss = MR_list_tail(neglitss);
      poslitss = MR_list_tail(poslitss);
   }


   /* create variable and constraint for branching on sum of active vars */
   /* prevents aggregations, so not currently used */

   /* SCIP_CALL( SCIPcreateConsBasicLinear(scip, &cons, "sumcons", 0, NULL, NULL, 0.0, 0.0) ); */
   /* nvars = 0; */
   /* for( i = 0; i < probdata->nvars; ++i) */
   /*    if( !MR_once_only(probdata->atom_store,i) ) */
   /*    { */
   /*       SCIP_CALL( SCIPaddCoefLinear(scip, cons, probdata->vars[i], 1) ); */
   /*       nvars++; */
   /*    } */
   
   /* SCIP_CALL( SCIPcreateVarBasic(scip, &var, "atomcount", 0.0, nvars, 0.0, SCIP_VARTYPE_INTEGER) ); */
   /* SCIP_CALL( SCIPaddVar(scip, var) ); */
   /* SCIP_CALL( SCIPmarkDoNotMultaggrVar(scip, var) ); */
   /* SCIP_CALL( SCIPchgVarBranchPriority(scip, var, 10) ); */
   /* SCIP_CALL( SCIPaddCoefLinear(scip, cons,  var, -1) ); */
   /* SCIP_CALL( SCIPaddCons(scip, cons) ); */
   /* /\*SCIP_CALL( SCIPprintCons(scip, cons, NULL)  );*\/ */
   /* SCIP_CALL( SCIPreleaseCons(scip, &cons) ); */

   /* get list of names of first-order clauses from Mercury */
   MR_delayed_clauses(&clausenames);

   /* create all first-order linear constraints */
   while ( !MR_list_is_empty(clausenames) )
   {
      /* create first-order constraint */
      /* just need the name of the clause! */
      SCIP_CALL( SCIPcreateConsBasicFolinear(scip, &cons, 
            (char*) MR_list_head(clausenames)) );
      SCIP_CALL( SCIPaddCons(scip, cons) );
      SCIP_CALL( SCIPreleaseCons(scip, &cons) );

      clausenames = MR_list_tail(clausenames);
   }

   /* solve the problem instance */
   SCIP_CALL( SCIPsolve(scip) );

   /* print the solution to standard output */
   SCIP_CALL( SCIPprintBestSol(scip, NULL, FALSE) );

   /* print solving statistics */
   /* SCIP_CALL( SCIPprintStatistics(scip, NULL) ); */

   /* and tidy up */
   SCIP_CALL( SCIPfree(&scip) );

   BMScheckEmptyMemory();

   return mercury_terminate();

}
