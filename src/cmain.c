#include <string.h>

#include <scip/scip.h>
#include <scip/scipdefplugins.h>


/** main function (just for testing at present ) */
static
int makesolveprob(
   char*                     filename
   )
{
   SCIP* scip = NULL;
   SCIP_CONS* cons;
   SCIP_VAR* var;

   
   /* initialize SCIP */
   SCIP_CALL( SCIPcreate(&scip) );

   /* include default SCIP plugins */
   SCIP_CALL( SCIPincludeDefaultPlugins(scip) );

   SCIP_CALL( SCIPcreateProbBasic(scip, "dummy") );


   SCIP_CALL( SCIPsetObjsense(scip, SCIP_OBJSENSE_MAXIMIZE) );

   /* add a variable */
   SCIP_CALL( SCIPcreateVarBasic(scip, &var, "todo", 0.0, 10.0, 20, SCIP_VARTYPE_INTEGER) );
   SCIP_CALL( SCIPaddVar(scip, var) );
   
   /* add a constraint */
   SCIP_CALL( SCIPcreateConsBasicLinear(scip, &cons, "todo", 0, NULL, NULL, 0, 5) );
   SCIP_CALL( SCIPaddCoefLinear(scip, cons, var, 1) );
   SCIP_CALL( SCIPaddCons(scip, cons) );
   SCIP_CALL( SCIPreleaseCons(scip, &cons) );

   /* solve the model */
   SCIP_CALL( SCIPsolve(scip) );

   SCIP_CALL( SCIPprintBestSol(scip, NULL, FALSE) );

   /* SCIP_CALL( SCIPprintStatistics(scip, NULL) ); */

   SCIP_CALL( SCIPfree(&scip) );

   return 0;
}

/** main function (just for testing at present ) */
int main(
   int                        argc,          /**< number of arguments from the shell */
   char**                     argv           /**< array of shell arguments */
   )
{
   int result;

   if( argc != 2 )
   {
      printf("Expected exactly one argument. Aborting\n");
      return 1;
   }

   result = makesolveprob(argv[1]);
   
   BMScheckEmptyMemory();

   return result;
}   
