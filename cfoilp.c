#include <string.h>
#include <stdio.h>
#include <stdlib.h>


#include <scip/scip.h>
#include <scip/scipdefplugins.h>

#include "cons_folinear.h"
#include "pricer_fovars.h"
#include "cfoilp.h"
/*#include "pricer_fovars.h"*/


/*
** This header file is part of the stand-alone interface.
** It contains declarations for mercury_init() and mercury_terminate(),
** which are used to respectively start and shutdown the Mercury runtime.
*/
#include "mfoilp_int.h"

#include "mercury_stuff.h"





/** main function (just for testing at present ) */
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

   SCIP_Real obj;
   MR_String varname;     /* SCIP happy to use the Mercury pointer directly */
   MR_String consname;    /* SCIP happy to use the Mercury pointer directly */

   SCIP_VAR* var;
   
   MR_IntList neglits;
   MR_IntList poslits;

   SCIP_VAR* negvar;

   SCIP_CONS* cons;

   MR_IntList vars_indices_infolinear;
   int n_varsinfolinear;
   SCIP_VAR** varsinfolinear;
   MR_IntList mr_up;
   MR_IntList mr_down;
   SCIP_Bool* up;
   SCIP_Bool* down;

   int i;

   mercury_init(argc, argv, &stack_bottom);

   /* initialize SCIP */
   SCIP_CALL( SCIPcreate(&scip) );

   /* include default SCIP plugins */
   SCIP_CALL( SCIPincludeDefaultPlugins(scip) );

   /* allocate memory */
   SCIP_CALL( SCIPallocMemory(scip, &probdata) );


   SCIP_CALL( SCIPcreateProb(scip, "folilp", NULL, NULL, NULL,
         NULL, NULL, NULL, probdata) );


   MR_initial_constraints(&atomstore,&objectives,&varnames,&consnames,&neglitss,&poslitss);

   /* initialise probdata */

   probdata->nvars = 0;
   probdata->vars = NULL;
   probdata->vars_len = VAR_BLOCKSIZE;
   SCIP_CALL( SCIPallocMemoryArray(scip, &(probdata->vars), probdata->vars_len) );
   probdata->atom_store = atomstore;

   /* create binary variables in constraints using "objectives" list */

   while ( !MR_list_is_empty(objectives) ) 
   {
      obj = MR_word_to_float(MR_list_head(objectives));
      varname =    (MR_String) MR_list_head(varnames);
      SCIP_CALL( SCIPcreateVarBasic(scip, &var, varname, 0.0, 1.0, obj, SCIP_VARTYPE_BINARY) );
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
      consname = (MR_String)  MR_list_head(consnames);
      neglits =  MR_list_head(neglitss);
      poslits =  MR_list_head(poslitss);
      
      SCIP_CALL( SCIPcreateConsBasicLogicor(scip, &cons, consname, 0, NULL) );
      
      while ( !MR_list_is_empty(neglits) )
      {
         var = probdata->vars[MR_list_head(neglits)];
         SCIP_CALL( SCIPgetNegatedVar(scip,var,&negvar) );
         SCIP_CALL( SCIPaddCoefLogicor(scip, cons, negvar) );
         neglits =  MR_list_head(neglits);
      }

      while ( !MR_list_is_empty(poslits) )
      {
         var = probdata->vars[MR_list_head(poslits)];
         SCIP_CALL( SCIPaddCoefLogicor(scip, cons, var) );
         poslits =  MR_list_head(poslits);
      }

      SCIP_CALL( SCIPaddCons(scip, cons) );
      /*SCIP_CALL(  SCIPprintCons(scip, cons, NULL)  );*/
      SCIP_CALL( SCIPreleaseCons(scip, &cons) );

      consnames = MR_list_tail(consnames);
      neglitss = MR_list_tail(neglitss);
      poslitss = MR_list_tail(poslitss);
   }

   /* include first-order linear constraint handler */
   SCIP_CALL( SCIPincludeConshdlrFolinear(scip) );

   /* find out which, if any, of the variables in the initial clauses are
      also involved in the delayed clauses
   */

   MR_varsinfolinear(probdata->nvars, probdata->atom_store, &vars_indices_infolinear, &n_varsinfolinear, &mr_down, &mr_up);

   SCIP_CALL( SCIPallocMemoryArray(scip, &varsinfolinear, probdata->nvars) );
   SCIP_CALL( SCIPallocMemoryArray(scip, &down, probdata->nvars) );
   SCIP_CALL( SCIPallocMemoryArray(scip, &up, probdata->nvars) );
   
   for( i = 0; i < n_varsinfolinear; ++i )
   {
      varsinfolinear[i] = probdata->atom_store[vars_indices_infolinear[i]];
      down[i] = MR_list_head(mr_down) == 1 ? TRUE : FALSE;
      up[i] = MR_list_head(mr_up) == 1 ? TRUE : FALSE;
      mr_down = MR_list_tail(mr_down);
      mr_up = MR_list_tail(mr_up);
   }


   /* create first-order constraint */
   SCIP_CALL( SCIPcreateConsBasicFolinear(scip, &cons, "global_folinear", varsinfolinear, n_varsinfolinear, down, up) );
   SCIP_CALL( SCIPaddCons(scip, cons) );
   /*SCIP_CALL( SCIPreleaseCons(scip, &cons) );*/

   SCIPfreeMemoryArray(scip,&varsinfolinear);
   SCIPfreeMemoryArray(scip,&down);
   SCIPfreeMemoryArray(scip,&up);

   /* solve the model */
   SCIP_CALL( SCIPsolve(scip) );

   SCIP_CALL( SCIPprintBestSol(scip, NULL, FALSE) );

   /* SCIP_CALL( SCIPprintStatistics(scip, NULL) ); */

   SCIP_CALL( SCIPfree(&scip) );

   BMScheckEmptyMemory();

   return mercury_terminate();

}
