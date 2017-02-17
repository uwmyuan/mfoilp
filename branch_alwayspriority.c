/**@file   branch_alwayspriority.c
 * @brief  alwayspriority branching rule
 * @author symbreak
 */

/*---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2*/

#include <assert.h>

#include "branch_alwayspriority.h"


#define BRANCHRULE_NAME            "alwayspriority"
#define BRANCHRULE_DESC            "always branch on the highest priority unfixed variable"
#define BRANCHRULE_PRIORITY        20000
#define BRANCHRULE_MAXDEPTH        -1
#define BRANCHRULE_MAXBOUNDDIST    1.0


/*
 * Data structures
 */

/* TODO: fill in the necessary branching rule data */

/** branching rule data */
struct SCIP_BranchruleData
{
   int dummy;          /* might put something useful here later */
};


/*
 * Local methods
 */

/* selects the highest priority pseudo candidate (breaking ties arbitrarily) */
static
SCIP_RETCODE getHighestPrioPSCanVariable(
   SCIP*                 scip,               /**< SCIP data structure */
   SCIP_VAR**            bestcand            /**< buffer to store pointer to best candidate */
   )
{

   SCIP_VAR** pseudocands;
   int npseudocands;
   int cand;
   int priority;
   int bestpriority;
   SCIP_VAR* var;
   SCIP_VAR* bestvar;

   assert(scip != NULL);
   assert(bestcand != NULL);

   /* get branching candidates (ignoring LP) */
   SCIP_CALL( SCIPgetPseudoBranchCands(scip, &pseudocands, NULL, &npseudocands) );
   assert(npseudocands > 0);

   bestpriority = 0;
   bestvar = NULL;
   for( cand = 0; cand < npseudocands; cand++ )
   {
      var = pseudocands[cand];
      priority = SCIPvarGetBranchPriority(var);
      if( priority > bestpriority )
      {
         bestvar = var;
         bestpriority = priority;
      }
   }

   *bestcand = bestvar;

   return SCIP_OKAY;

}


/*
 * Callback methods of branching rule
 */

/* TODO: Implement all necessary branching rule methods. The methods with an #if 0 ... #else #define ... are optional */


/** copy method for branchrule plugins (called when SCIP copies plugins) */
static
SCIP_DECL_BRANCHCOPY(branchCopyAlwayspriority)
{

   assert(scip != NULL);
   assert(branchrule != NULL);
   assert(strcmp(SCIPbranchruleGetName(branchrule), BRANCHRULE_NAME) == 0);

   /* call inclusion method of branchrule */
   SCIP_CALL( SCIPincludeBranchruleAlwayspriority(scip) );


   
   return SCIP_OKAY;
}


/** destructor of branching rule to free user data (called when SCIP is exiting) */
static
SCIP_DECL_BRANCHFREE(branchFreeAlwayspriority)
{

   SCIP_BRANCHRULEDATA* branchruledata;

   /* free branching rule data */
   branchruledata = SCIPbranchruleGetData(branchrule);
   SCIPfreeMemory(scip, &branchruledata);
   SCIPbranchruleSetData(branchrule, NULL);

   return SCIP_OKAY;
}



/** initialization method of branching rule (called after problem was transformed) */
static
SCIP_DECL_BRANCHINIT(branchInitAlwayspriority)
{

   SCIP_BRANCHRULEDATA* branchruledata;

   branchruledata = SCIPbranchruleGetData(branchrule);
   assert(branchruledata != NULL);

   /* set the dummy value */
   branchruledata->dummy = 0;

   return SCIP_OKAY;
}



/** branching execution method for fractional LP solutions */
static
SCIP_DECL_BRANCHEXECLP(branchExeclpAlwayspriority)
{
   SCIP_VAR* bestcand;

   assert(branchrule != NULL);
   assert(strcmp(SCIPbranchruleGetName(branchrule), BRANCHRULE_NAME) == 0);
   assert(scip != NULL);
   assert(result != NULL);

   SCIPdebugMessage("Execlp method of alwayspriority in depth %d\n", SCIPgetDepth(scip));

   SCIP_CALL( getHighestPrioPSCanVariable(scip, &bestcand) );

   if( bestcand == NULL )
   {
      *result = SCIP_DIDNOTRUN;
   }
   else
   {
      SCIP_CALL( SCIPbranchVar(scip, bestcand, NULL, NULL, NULL) );
      *result = SCIP_BRANCHED;
   }

   return SCIP_OKAY;
}



/** branching execution method for not completely fixed pseudo solutions */
static
SCIP_DECL_BRANCHEXECPS(branchExecpsAlwayspriority)
{
   SCIP_VAR* bestcand;
   
   assert(branchrule != NULL);
   assert(strcmp(SCIPbranchruleGetName(branchrule), BRANCHRULE_NAME) == 0);
   assert(scip != NULL);
   assert(result != NULL);

   SCIPdebugMessage("Execps method of random branching\n");

   SCIP_CALL( getHighestPrioPSCanVariable(scip, &bestcand) );

   if( bestcand == NULL )
   {
      *result = SCIP_DIDNOTRUN;
   }
   else
   {
      SCIP_CALL( SCIPbranchVar(scip, bestcand, NULL, NULL, NULL) );
      *result = SCIP_BRANCHED;
   }

   return SCIP_OKAY;
}

/*
 * branching rule specific interface methods
 */

/** creates the alwayspriority branching rule and includes it in SCIP */
SCIP_RETCODE SCIPincludeBranchruleAlwayspriority(
   SCIP*                 scip                /**< SCIP data structure */
   )
{
   SCIP_BRANCHRULEDATA* branchruledata;
   SCIP_BRANCHRULE* branchrule;

   /* create alwayspriority branching rule data */
   SCIP_CALL( SCIPallocMemory(scip, &branchruledata) );
   branchruledata->dummy = 0;

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
   SCIP_CALL( SCIPsetBranchruleCopy(scip, branchrule, branchCopyAlwayspriority) );
   SCIP_CALL( SCIPsetBranchruleFree(scip, branchrule, branchFreeAlwayspriority) );
   SCIP_CALL( SCIPsetBranchruleInit(scip, branchrule, branchInitAlwayspriority) );
   SCIP_CALL( SCIPsetBranchruleExecLp(scip, branchrule, branchExeclpAlwayspriority) );
   SCIP_CALL( SCIPsetBranchruleExecPs(scip, branchrule, branchExecpsAlwayspriority) );


   /* add alwayspriority branching rule parameters */
   /* TODO: (optional) add branching rule specific parameters with SCIPaddTypeParam() here */

   return SCIP_OKAY;
}
