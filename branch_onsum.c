/**@file   branch_onsum.c
 * @brief  onsum branching rule
 * @author symbreak
 */

/*---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2*/
#define SCIP_DEBUG
#include <assert.h>

#include <scip/scip.h>
#include <scip/scipdefplugins.h>
#include "branch_onsum.h"


#define BRANCHRULE_NAME            "onsum"
#define BRANCHRULE_DESC            "branch on number of binary variables (created in root node) set to true"
#define BRANCHRULE_PRIORITY        30000
#define BRANCHRULE_MAXDEPTH        -1
#define BRANCHRULE_MAXBOUNDDIST    1.0

#define DEFAULT_LBNBINVARS         0      /**< lower bound on number of binary variables (created before first branch) set to TRUE */
#define DEFAULT_UBNBINVARS         -1     /**< upper bound on number of binary variables (created before first branch) set to TRUE */


/*
 * Data structures
 */

/** branching rule data */
struct SCIP_BranchruleData
{
   SCIP_VAR* particularvar;
   int lbnbinvars;
   int ubnbinvars;
};


/*
 * Local methods
 */

static
SCIP_RETCODE setparticular(
   SCIP* scip,
   SCIP_BRANCHRULEDATA* branchruledata
   )
{
   
   /* binary variables always come first in the active variables array */
   int nvars = SCIPgetNBinVars(scip);
   SCIP_VAR** vars = SCIPgetVars(scip);
   SCIP_Real* vals;
   SCIP_VAR* var;
   int i;
   SCIP_CONS* cons;

   assert(scip != NULL);
   
   SCIP_CALL( SCIPallocMemoryArray(scip, &vals, nvars) );
   for( i = 0; i < nvars; ++i)
      vals[i] = 1.0;

   SCIP_CALL( SCIPcreateVarBasic(scip, &var, "atomcount0",
         branchruledata->lbnbinvars, branchruledata->ubnbinvars == -1 ? nvars : branchruledata->ubnbinvars, 0.0, SCIP_VARTYPE_INTEGER) );
   SCIP_CALL( SCIPaddVar(scip, var) );
   
   SCIP_CALL( SCIPcreateConsBasicLinear(scip, &cons, "sumcons", nvars, vars, vals, 0.0, 0.0) );
   SCIP_CALL( SCIPaddCoefLinear(scip, cons, var, -1.0) );
   SCIP_CALL( SCIPaddCons(scip, cons) );
   SCIP_CALL( SCIPreleaseCons(scip, &cons) );
   branchruledata->particularvar = var;

   SCIPfreeMemoryArray(scip, &vals);

   return SCIP_OKAY;
}

/*
 * Callback methods of branching rule
 */

/* TODO: Implement all necessary branching rule methods. The methods with an #if 0 ... #else #define ... are optional */


/** copy method for branchrule plugins (called when SCIP copies plugins) */
static
SCIP_DECL_BRANCHCOPY(branchCopyOnsum)
{
   
   assert(scip != NULL);
   assert(branchrule != NULL);
   assert(strcmp(SCIPbranchruleGetName(branchrule), BRANCHRULE_NAME) == 0);

   /* call inclusion method of branchrule */
   SCIP_CALL( SCIPincludeBranchruleOnsum(scip) );

   return SCIP_OKAY;
}


/** destructor of branching rule to free user data (called when SCIP is exiting) */
static
SCIP_DECL_BRANCHFREE(branchFreeOnsum)
{

   SCIP_BRANCHRULEDATA* branchruledata;

   /* free branching rule data */
   branchruledata = SCIPbranchruleGetData(branchrule);
   SCIPfreeMemory(scip, &branchruledata);
   SCIPbranchruleSetData(branchrule, NULL);

   return SCIP_OKAY;
}

/** branching execution method for fractional LP solutions */
static
SCIP_DECL_BRANCHEXECLP(branchExeclpOnsum)
{
   SCIP_VAR* var;
   SCIP_BRANCHRULEDATA* branchruledata;

   assert(branchrule != NULL);
   assert(strcmp(SCIPbranchruleGetName(branchrule), BRANCHRULE_NAME) == 0);
   assert(scip != NULL);
   assert(result != NULL);

   SCIPdebugMessage("Execlp method of onsum in depth %d\n", SCIPgetDepth(scip));

   branchruledata = SCIPbranchruleGetData(branchrule);
   
   if( SCIPgetDepth(scip) == 0 )
      SCIP_CALL( setparticular(scip, branchruledata) );

   var = branchruledata->particularvar;

   if( !SCIPisEQ(scip, SCIPvarGetUbLocal(var), SCIPvarGetLbLocal(var)) )
   {
      SCIPdebugMessage(" Branching on <%s>, lb=%g, ub=%g\n",
         SCIPvarGetName(var),  SCIPvarGetLbLocal(var), SCIPvarGetUbLocal(var) );
      SCIP_CALL( SCIPbranchVar(scip, var, NULL, NULL, NULL) );
      *result = SCIP_BRANCHED;
   }
   else
   {
      SCIPdebugMessage(" Did not branch on <%s>, lb=%g, ub=%g\n",
         SCIPvarGetName(var),  SCIPvarGetLbLocal(var), SCIPvarGetUbLocal(var) );
      *result = SCIP_DIDNOTRUN;
   }
   
   return SCIP_OKAY;
}



/** branching execution method for not completely fixed pseudo solutions */
static
SCIP_DECL_BRANCHEXECPS(branchExecpsOnsum)
{
   SCIP_VAR* var;
   SCIP_BRANCHRULEDATA* branchruledata;

   assert(branchrule != NULL);
   assert(strcmp(SCIPbranchruleGetName(branchrule), BRANCHRULE_NAME) == 0);
   assert(scip != NULL);
   assert(result != NULL);

   SCIPdebugMessage("Execps method of onsum in depth %d\n", SCIPgetDepth(scip));

   branchruledata = SCIPbranchruleGetData(branchrule);

   if( SCIPgetDepth(scip) == 0 )
      SCIP_CALL( setparticular(scip, &(branchruledata->particularvar)) );
   
   var = branchruledata->particularvar;

   if( !SCIPisEQ(scip, SCIPvarGetUbLocal(var), SCIPvarGetLbLocal(var)) )
   {
      SCIPdebugMessage(" Branching on <%s>, lb=%g, ub=%g\n",
         SCIPvarGetName(var),  SCIPvarGetLbLocal(var), SCIPvarGetUbLocal(var) );
      SCIP_CALL( SCIPbranchVar(scip, var, NULL, NULL, NULL) );
      *result = SCIP_BRANCHED;
   }
   else
   {
      SCIPdebugMessage(" Did not branch on <%s>, lb=%g, ub=%g\n",
         SCIPvarGetName(var),  SCIPvarGetLbLocal(var), SCIPvarGetUbLocal(var) );
      *result = SCIP_DIDNOTRUN;
   }
   
   return SCIP_OKAY;
}

/*
 * branching rule specific interface methods
 */

/** creates the onsum branching rule and includes it in SCIP */
SCIP_RETCODE SCIPincludeBranchruleOnsum(
   SCIP*                 scip               /**< SCIP data structure */
   )
{
   SCIP_BRANCHRULEDATA* branchruledata;
   SCIP_BRANCHRULE* branchrule;

   /* create onsum branching rule data */
   SCIP_CALL( SCIPallocMemory(scip, &branchruledata) );
   branchruledata->particularvar = NULL;

   /* TODO: (optional) create branching rule specific data here */

   branchrule = NULL;

   /* include branching rule */

   /* use SCIPincludeBranchruleBasic() plus setter functions if you want to set callbacks one-by-one and your code should
    * compile independent of new callbacks being added in future SCIP versions
    */
   SCIP_CALL( SCIPincludeBranchruleBasic(scip, &branchrule, BRANCHRULE_NAME, BRANCHRULE_DESC, BRANCHRULE_PRIORITY,
         BRANCHRULE_MAXDEPTH, BRANCHRULE_MAXBOUNDDIST, branchruledata) );

   assert(branchrule != NULL);

   /* set non fundamental callbacks via setter functions */
   SCIP_CALL( SCIPsetBranchruleCopy(scip, branchrule, branchCopyOnsum) );
   SCIP_CALL( SCIPsetBranchruleFree(scip, branchrule, branchFreeOnsum) );
   SCIP_CALL( SCIPsetBranchruleExecLp(scip, branchrule, branchExeclpOnsum) );
   SCIP_CALL( SCIPsetBranchruleExecPs(scip, branchrule, branchExecpsOnsum) );

   SCIP_CALL( SCIPaddIntParam(scip,
         "branching/" BRANCHRULE_NAME "/lbnbinvars",
         "lower bound on the number of binary variables (created before first branch) set to TRUE",
         &branchruledata->lbnbinvars, FALSE, DEFAULT_LBNBINVARS, 0, INT_MAX, NULL, NULL) );

   SCIP_CALL( SCIPaddIntParam(scip,
         "branching/" BRANCHRULE_NAME "/ubnbinvars",
         "upper bound on the number of binary variables (created before first branch) set to TRUE (-1 is no bound)",
         &branchruledata->ubnbinvars, FALSE, DEFAULT_UBNBINVARS, -1, INT_MAX, NULL, NULL) );


   
   /* add onsum branching rule parameters */
   /* TODO: (optional) add branching rule specific parameters with SCIPaddTypeParam() here */

   return SCIP_OKAY;
}
