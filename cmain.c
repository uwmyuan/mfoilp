#include <string.h>
#include <stdio.h>
#include <stdlib.h>


#include <scip/scip.h>
#include <scip/scipdefplugins.h>

#include "cons_folinear.h"


/*
** This header file is part of the stand-alone interface.
** It contains declarations for mercury_init() and mercury_terminate(),
** which are used to respectively start and shutdown the Mercury runtime.
*/
#include "mercury_lib_int.h"

#include "mercury_stuff.h"

#define VAR_BLOCKSIZE 10;

struct SCIP_ProbData
{
   int          nvars;         /**< number of variables in the problem */
   SCIP_VAR**   vars;          /**< variables in the problem */
   int          vars_len;      /**< length of vars array */
   MR_AtomStore atom_store;    /**< Mercury bimap between atoms and their indices */
};

/** main function (just for testing at present ) */
int main(
   int                        argc,          /**< number of arguments from the shell */
   char**                     argv           /**< array of shell arguments */
   )
{
   void *stack_bottom;

   MR_AtomStore atomstore;
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

   /* allocate memory */
   SCIP_CALL( SCIPallocMemory(scip, &probdata) );


   SCIP_CALL( SCIPcreateProb(scip, "folilp", NULL, NULL, NULL,
         NULL, NULL, NULL, probdata) );


   SCIP_CALL( SCIPsetObjsense(scip, SCIP_OBJSENSE_MAXIMIZE) );

   /* Call Mercury to create variables */

   MR_initial_variables(&atomstore,    /* don't need this to make variables
                               but needed for bookkeeping */
      &idents,             /* index for each variable to be created */
      &names,              /* name for each variable to be created */
      &lbs,                /* lower bound for each variable to be created */
      &ubs,                /* upper bound for each variable to be created */
      &vartypes,           /* variable type for each variable to be created */
      &objs);              /* objective coeff for each variable to be created */


   /* add Mercury variables to SCIP instance */
   probdata->atom_store = atomstore;
   probdata->nvars = 0;
   probdata->vars_len = VAR_BLOCKSIZE;
   SCIP_CALL( SCIPallocMemoryArray(scip, &(probdata->vars), probdata->vars_len) );

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
   SCIP_CALL( SCIPcreateConsBasicFolinear(scip, &cons, "todo", 
         probdata->nvars,probdata->vars,probdata->atom_store) );
   SCIP_CALL( SCIPaddCons(scip, cons) );
   SCIP_CALL( SCIPreleaseCons(scip, &cons) );


   MR_initial_constraints(atomstore,&names,&lbs,&finlbs,&coeffss,&varss,&ubs,&finubs);

   while ( !MR_list_is_empty(lbs) )
   {
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
      SCIP_CALL( SCIPaddCons(scip, cons) );
      /*SCIP_CALL(  SCIPprintCons(scip, cons, NULL)  ); */
      SCIP_CALL( SCIPreleaseCons(scip, &cons) );

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
