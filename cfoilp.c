#include <string.h>
#include <stdio.h>
#include <stdlib.h>


#include <scip/scip.h>
#include <scip/scipdefplugins.h>

#include "cons_folinear.h"
#include "pricer_dummy.h"
#include "cfoilp.h"
#include "branch_alwayspriority.h"
#include "branch_alwaysparticular.h"


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


/** main function */
int main(
   int                        argc,          /**< number of arguments from the shell */
   char**                     argv           /**< array of shell arguments */
   )
{
   void *stack_bottom;

   SCIP* scip = NULL;
   SCIP_PROBDATA*  probdata = NULL;

   MR_FloatList objectives;
   MR_StringList varnames;
   MR_StringList consnames;
   MR_IntListList neglitss;
   MR_IntListList poslitss;

   SCIP_VAR* var;
   
   MR_IntList neglits;
   MR_IntList poslits;

   SCIP_VAR* negvar;

   SCIP_CONS* cons;

   int i;

   MR_StringList clausenames;

   SCIP_VAR* clausevars[100];

   const char paramfile[] = "mfoilp.set";

   SCIP_Bool pricer;
   SCIP_Bool write_presolved;
   
   mercury_init(argc, argv, &stack_bottom);

   /* initialize SCIP */
   SCIP_CALL( SCIPcreate(&scip) );

   /* include default SCIP plugins */
   SCIP_CALL( SCIPincludeDefaultPlugins(scip) );

   /* include first-order linear constraint handler */
   SCIP_CALL( SCIPincludeConshdlrFolinear(scip) );

   /* include always priority branching rule */
   SCIP_CALL( SCIPincludeBranchruleAlwayspriority(scip) );

   
   SCIP_CALL( SCIPaddBoolParam(scip, "mfoilp/pricer", "is there a pricer?", &pricer, FALSE, TRUE, NULL, NULL) );
   SCIP_CALL( SCIPaddBoolParam(scip, "mfoilp/write_presolved", "whether to write out presolved problem", &write_presolved, FALSE, FALSE, NULL, NULL) );

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

   /* include dummy pricer  */
   /* SCIP_CALL( SCIPgetBoolParam(scip, "mfoilp/pricer", &pricer) );  */
   if( pricer )
      SCIP_CALL( SCIPincludePricerDummy(scip) );
   
   /* allocate memory */
   SCIP_CALL( SCIPallocMemory(scip, &probdata) );

   SCIP_CALL( SCIPcreateProb(scip, "folilp", probdelorigFOILP, NULL, NULL,
         NULL, NULL, NULL, probdata) );

   /* activates dummy pricer  */
   if( pricer )
      SCIP_CALL( SCIPactivatePricer(scip, SCIPfindPricer(scip, "dummy")) );


   MR_initial_constraints(&objectives,&varnames,&consnames,&neglitss,&poslitss);

   /* initialise probdata */

   probdata->nvars = 0;
   probdata->vars = NULL;
   probdata->vars_len = VAR_BLOCKSIZE;
   SCIP_CALL( SCIPallocMemoryArray(scip, &(probdata->vars), probdata->vars_len) );

   /* create binary variables in constraints using "objectives" list */

   while ( !MR_list_is_empty(objectives) ) 
   {
      SCIP_CALL( SCIPcreateVarBasic(scip, &var, 
            (char *) MR_list_head(varnames), 
            0.0, 1.0, 
            (SCIP_Real) MR_word_to_float(MR_list_head(objectives)), 
            SCIP_VARTYPE_BINARY) );
      SCIP_CALL( SCIPaddVar(scip, var) );

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

   /* now add the initial constraints */
   
   while ( !MR_list_is_empty(consnames) )
   {
      neglits =  MR_list_head(neglitss);
      poslits =  MR_list_head(poslitss);

      i = 0;

      while ( !MR_list_is_empty(neglits) )
      {
         var = probdata->vars[(int) MR_list_head(neglits)];
         SCIP_CALL( SCIPgetNegatedVar(scip,var,&negvar) );
         clausevars[i++] = negvar;
         neglits =  MR_list_tail(neglits);
      }

      while ( !MR_list_is_empty(poslits) )
      {
         var = probdata->vars[(int) MR_list_head(poslits)];
         clausevars[i++] = var;
         poslits =  MR_list_tail(poslits);
      }

      SCIP_CALL( SCIPcreateConsBasicLogicor(scip, &cons, 
            (char *)  MR_list_head(consnames), 
            i, clausevars) );
      SCIP_CALL( SCIPaddCons(scip, cons) );
      /*SCIP_CALL( SCIPprintCons(scip, cons, NULL)  );*/
      SCIP_CALL( SCIPreleaseCons(scip, &cons) );

      consnames = MR_list_tail(consnames);
      neglitss = MR_list_tail(neglitss);
      poslitss = MR_list_tail(poslitss);
   }


   /* get list of names of first-order clauses */
   
   MR_delayed_clauses(&clausenames);

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

   /* add in atomcount variable */

   SCIP_CALL( SCIPcreateVarBasic(scip, &var, "atomcount", 0, SCIPinfinity(scip), 0.0, SCIP_VARTYPE_INTEGER) );
   SCIP_CALL( SCIPaddVar(scip, var) );
   SCIP_CALL( SCIPchgVarBranchPriority(scip, var, 10) );

   SCIP_CALL( SCIPcreateConsBasicLinear(scip, &cons, "sumcons", 0, NULL, NULL, 0.0, 0.0) );
   SCIP_CALL( SCIPsetConsModifiable(scip, cons, TRUE) );
   SCIP_CALL( SCIPaddCoefLinear(scip, cons, var, -1.0) );
   SCIP_CALL( SCIPaddCons(scip, cons) );
   probdata->atomcount_cons = cons;
   /*SCIP_CALL( SCIPprintCons(scip, cons, NULL)  );*/
   SCIP_CALL( SCIPreleaseCons(scip, &cons) );

   /* include always priority branching rule */
   SCIP_CALL( SCIPincludeBranchruleAlwaysparticular(scip, var) );

   
   /* solve the model */

   if( write_presolved )
   {
      SCIPpresolve(scip);
      SCIP_CALL( SCIPwriteTransProblem(scip, "prob.lp", NULL, FALSE) );
   }
   
   SCIP_CALL( SCIPsolve(scip) );

   SCIP_CALL( SCIPprintBestSol(scip, NULL, FALSE) );

   /* SCIP_CALL( SCIPprintStatistics(scip, NULL) ); */

   SCIP_CALL( SCIPfree(&scip) );

   BMScheckEmptyMemory();

   return mercury_terminate();

}
