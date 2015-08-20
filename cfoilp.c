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

   MR_AtomStore atomstore;
   MR_ConsStore consstore;
   MR_RowStore rowstore;
   MR_IntList idents;
   MR_StringList names;
   MR_FloatList lbs;
   MR_IntList finlbs;
   MR_FloatList ubs;
   MR_IntList finubs;
   MR_IntList vartypes;
   MR_FloatList objs;
   
   MR_FloatListList coeffss;
   MR_IntListList varss;

   MR_FloatList coeffs;
   MR_IntList vars;

   SCIP* scip = NULL;
   SCIP_CONS* cons;
   SCIP_VAR* var;

   int ident;
   MR_String name;
   SCIP_Real lb;
   int finlb;
   SCIP_Real ub;
   int finub;   
   SCIP_Real obj;
   int vartype;

   SCIP_Real coeff;   

   SCIP_PROBDATA*  probdata;

   mercury_init(argc, argv, &stack_bottom);

   /* initialize SCIP */
   SCIP_CALL( SCIPcreate(&scip) );

   /* include default SCIP plugins */
   SCIP_CALL( SCIPincludeDefaultPlugins(scip) );

   /* include fovars pricer  */
   SCIP_CALL( SCIPincludePricerFovars(scip) );


   /* allocate memory */
   SCIP_CALL( SCIPallocMemory(scip, &probdata) );


   SCIP_CALL( SCIPcreateProb(scip, "folilp", NULL, NULL, NULL,
         NULL, NULL, NULL, probdata) );

   /* activates fovars pricer  */
   SCIP_CALL( SCIPactivatePricer(scip, SCIPfindPricer(scip, "fovars")) );



   /*SCIP_CALL( SCIPsetObjsense(scip, SCIP_OBJSENSE_MAXIMIZE) );*/

   /* initialise probdata */

   probdata->nvars = 0;
   probdata->vars = NULL;
   probdata->vars_len = VAR_BLOCKSIZE;
   SCIP_CALL( SCIPallocMemoryArray(scip, &(probdata->vars), probdata->vars_len) );
   probdata->nconss = 0;
   probdata->conss = NULL;
   probdata->conss_len = VAR_BLOCKSIZE;
   SCIP_CALL( SCIPallocMemoryArray(scip, &(probdata->conss), probdata->conss_len) );
   probdata->nrows = 0;
   probdata->rows = NULL;
   probdata->rows_len = VAR_BLOCKSIZE;
   SCIP_CALL( SCIPallocMemoryArray(scip, &(probdata->rows), probdata->rows_len) );

   /* initialise to empty Mercury bimap */ 
   MR_initial_rows(&rowstore);
   probdata->row_store = rowstore;

   /* Call Mercury to create variables */

   MR_initial_variables(&atomstore,    /* don't need this to make variables
                               but needed for bookkeeping */
      &idents,             /* index for each variable to be created */
      &names,              /* name for each variable to be created */
      &lbs,                /* lower bound for each variable to be created */
      &ubs,                /* upper bound for each variable to be created */
      &vartypes,           /* variable type for each variable to be created */
      &objs);              /* objective coeff for each variable to be created */

   probdata->atom_store = atomstore;

   /* add Mercury variables to SCIP instance */

   while ( !MR_list_is_empty(idents) ) 
   {
      ident =   MR_list_head(idents);
      name =    (MR_String) MR_list_head(names);
      lb =      MR_word_to_float(MR_list_head(lbs));
      ub =      MR_word_to_float(MR_list_head(ubs));
      obj =     MR_word_to_float(MR_list_head(objs));
      vartype = MR_list_head(vartypes);

      /* throw error if idents are not listed as 0,1,...n */
      if( ident != probdata->nvars )
      {
         SCIPerrorMessage("Mercury did not return list of variable indices  correctly.\n");
         exit(1);
      }

      SCIP_CALL( SCIPcreateVarBasic(scip, &var, name, lb, ub, obj, vartype) );
      SCIP_CALL( SCIPaddVar(scip, var) );
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

   /* Call Mercury to create constraints */   


   /* include first-order linear constraint handler */
   SCIP_CALL( SCIPincludeConshdlrFolinear(scip) );

   /* create first-order constraint */
   SCIP_CALL( SCIPcreateConsBasicFolinear(scip, &cons, "global_folinear") );
   SCIP_CALL( SCIPaddCons(scip, cons) );
   SCIP_CALL( SCIPreleaseCons(scip, &cons) );


   MR_initial_constraints(atomstore,&consstore,&idents,&names,&lbs,&finlbs,&coeffss,&varss,&ubs,&finubs);

   probdata->cons_store = consstore;

   while ( !MR_list_is_empty(idents) )
   {
      ident =   MR_list_head(idents);

      /* throw error if idents are not listed as 0,1,...n */
      if( ident != probdata->nconss )
      {
         SCIPerrorMessage("Mercury did not return list of constraint indices  correctly.\n");
         exit(1);
      }


      coeffs = MR_list_head(coeffss);
      vars = MR_list_head(varss);
      name = (MR_String)  MR_list_head(names);

      finlb = MR_list_head(finlbs);
      if( finlb )
         lb = MR_word_to_float(MR_list_head(lbs));
      else
         lb = -SCIPinfinity(scip);

      finub = MR_list_head(finubs);
      if( finub )
         ub = MR_word_to_float(MR_list_head(ubs));
      else
         ub = SCIPinfinity(scip);

      /* add a constraint */
      SCIP_CALL( SCIPcreateConsBasicLinear(scip, &cons, name, 0, NULL, NULL, lb, ub) );
      while ( !MR_list_is_empty(coeffs) )
      {
         coeff = MR_word_to_float(MR_list_head(coeffs));
         var = probdata->vars[MR_list_head(vars)];
         SCIP_CALL( SCIPaddCoefLinear(scip, cons, var, coeff) );
         coeffs = MR_list_tail(coeffs);
         vars = MR_list_tail(vars);

      }
      /* declare constraint modifiable for adding variables during pricing */
      SCIP_CALL( SCIPsetConsModifiable(scip, cons, TRUE) );

      SCIP_CALL( SCIPaddCons(scip, cons) );
      /*SCIP_CALL(  SCIPprintCons(scip, cons, NULL)  ); */
      SCIP_CALL( SCIPreleaseCons(scip, &cons) );

      if( !(ident < probdata->conss_len) )
      {
         probdata->conss_len += VAR_BLOCKSIZE;
         SCIP_CALL( SCIPreallocMemoryArray(scip, &(probdata->conss), probdata->conss_len) );
      }

      /* HERE is where constraint is added to probdata */
      probdata->conss[probdata->nconss++] = cons;

      idents = MR_list_tail(idents);
      coeffss = MR_list_tail(coeffss);
      varss = MR_list_tail(varss);
      names = MR_list_tail(names);
      lbs = MR_list_tail(lbs);
      finlbs = MR_list_tail(finlbs);
      ubs = MR_list_tail(ubs);
      finubs = MR_list_tail(finubs);
   }

   /* solve the model */
   SCIP_CALL( SCIPsolve(scip) );

   SCIP_CALL( SCIPprintBestSol(scip, NULL, FALSE) );

   /* SCIP_CALL( SCIPprintStatistics(scip, NULL) ); */

   SCIP_CALL( SCIPfree(&scip) );

   BMScheckEmptyMemory();

   return mercury_terminate();

}
