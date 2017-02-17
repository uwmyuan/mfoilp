/**@file   branch_alwaysparticular.c
 * @brief  alwaysparticular branching rule
 * @author symbreak
 */

/*---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2*/
#define SCIP_DEBUG
#include <assert.h>

#include "branch_alwaysparticular.h"


#define BRANCHRULE_NAME            "alwaysparticular"
#define BRANCHRULE_DESC            "always branch on the highest particular unfixed variable"
#define BRANCHRULE_PRIORITY        30000
#define BRANCHRULE_MAXDEPTH        -1
#define BRANCHRULE_MAXBOUNDDIST    1.0


/*
 * Data structures
 */

/* TODO: fill in the necessary branching rule data */

/** branching rule data */
struct SCIP_BranchruleData
{
   SCIP_VAR* particularvar;
};


/*
 * Local methods
 */



/*
 * Callback methods of branching rule
 */

/* TODO: Implement all necessary branching rule methods. The methods with an #if 0 ... #else #define ... are optional */


/** copy method for branchrule plugins (called when SCIP copies plugins) */
static
SCIP_DECL_BRANCHCOPY(branchCopyAlwaysparticular)
{
   SCIP_BRANCHRULEDATA* branchruledata;
   
   assert(scip != NULL);
   assert(branchrule != NULL);
   assert(strcmp(SCIPbranchruleGetName(branchrule), BRANCHRULE_NAME) == 0);

   branchruledata = SCIPbranchruleGetData(branchrule);

   /* call inclusion method of branchrule */
   SCIP_CALL( SCIPincludeBranchruleAlwaysparticular(scip, branchruledata->particularvar) );


   
   return SCIP_OKAY;
}


/** destructor of branching rule to free user data (called when SCIP is exiting) */
static
SCIP_DECL_BRANCHFREE(branchFreeAlwaysparticular)
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
SCIP_DECL_BRANCHEXECLP(branchExeclpAlwaysparticular)
{
   SCIP_VAR* var;
   SCIP_BRANCHRULEDATA* branchruledata;

   assert(branchrule != NULL);
   assert(strcmp(SCIPbranchruleGetName(branchrule), BRANCHRULE_NAME) == 0);
   assert(scip != NULL);
   assert(result != NULL);

   SCIPdebugMessage("Execlp method of alwaysparticular in depth %d\n", SCIPgetDepth(scip));

   branchruledata = SCIPbranchruleGetData(branchrule);
   var = branchruledata->particularvar;

   if( SCIPvarGetUbLocal(var) > SCIPvarGetLbLocal(var) )
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
SCIP_DECL_BRANCHEXECPS(branchExecpsAlwaysparticular)
{
   SCIP_VAR* var;
   SCIP_BRANCHRULEDATA* branchruledata;

   assert(branchrule != NULL);
   assert(strcmp(SCIPbranchruleGetName(branchrule), BRANCHRULE_NAME) == 0);
   assert(scip != NULL);
   assert(result != NULL);

   SCIPdebugMessage("Execps method of alwaysparticular in depth %d\n", SCIPgetDepth(scip));

   branchruledata = SCIPbranchruleGetData(branchrule);
   var = branchruledata->particularvar;

   if( SCIPvarGetUbLocal(var) > SCIPvarGetLbLocal(var) )
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

/** creates the alwaysparticular branching rule and includes it in SCIP */
SCIP_RETCODE SCIPincludeBranchruleAlwaysparticular(
   SCIP*                 scip,               /**< SCIP data structure */
   SCIP_VAR*             var                 /**< variable to branch on (if not fixed ) */
   )
{
   SCIP_BRANCHRULEDATA* branchruledata;
   SCIP_BRANCHRULE* branchrule;

   /* create alwaysparticular branching rule data */
   SCIP_CALL( SCIPallocMemory(scip, &branchruledata) );
   branchruledata->particularvar = var;

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
   SCIP_CALL( SCIPsetBranchruleCopy(scip, branchrule, branchCopyAlwaysparticular) );
   SCIP_CALL( SCIPsetBranchruleFree(scip, branchrule, branchFreeAlwaysparticular) );
   SCIP_CALL( SCIPsetBranchruleExecLp(scip, branchrule, branchExeclpAlwaysparticular) );
   SCIP_CALL( SCIPsetBranchruleExecPs(scip, branchrule, branchExecpsAlwaysparticular) );


   /* add alwaysparticular branching rule parameters */
   /* TODO: (optional) add branching rule specific parameters with SCIPaddTypeParam() here */

   return SCIP_OKAY;
}
