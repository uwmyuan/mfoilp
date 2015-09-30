/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/*                                                                           */
/*                  This file is part of the program and library             */
/*         SCIP --- Solving Constraint Integer Programs                      */
/*                                                                           */
/*    Copyright (C) 2002-2015 Konrad-Zuse-Zentrum                            */
/*                            fuer Informationstechnik Berlin                */
/*                                                                           */
/*  SCIP is distributed under the terms of the ZIB Academic License.         */
/*                                                                           */
/*  You should have received a copy of the ZIB Academic License              */
/*  along with SCIP; see the file COPYING. If not email to scip@zib.de.      */
/*                                                                           */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/**@file   pricer_dummy.c
 * @brief  dummy variable pricer
 * @author Tobias Achterberg
 */

/*---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2*/

#include <assert.h>

#include "pricer_dummy.h"


#define PRICER_NAME            "dummy"
#define PRICER_DESC            "variable pricer template"
#define PRICER_PRIORITY        0
#define PRICER_DELAY           TRUE     /* only call pricer if all problem variables have non-negative reduced costs */




/*
 * Data structures
 */

/* TODO: fill in the necessary variable pricer data */

/** variable pricer data */
struct SCIP_PricerData
{
};




/*
 * Local methods
 */

/* put your local methods here, and declare them static */




/*
 * Callback methods of variable pricer
 */

/* TODO: Implement all necessary variable pricer methods. The methods with an #if 0 ... #else #define ... are optional */

/** copy method for pricer plugins (called when SCIP copies plugins) */
#if 0
static
SCIP_DECL_PRICERCOPY(pricerCopyDummy)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of dummy variable pricer not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define pricerCopyDummy NULL
#endif

/** destructor of variable pricer to free user data (called when SCIP is exiting) */
#if 0
static
SCIP_DECL_PRICERFREE(pricerFreeDummy)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of dummy variable pricer not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define pricerFreeDummy NULL
#endif


/** initialization method of variable pricer (called after problem was transformed) */
#if 0
static
SCIP_DECL_PRICERINIT(pricerInitDummy)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of dummy variable pricer not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define pricerInitDummy NULL
#endif


/** deinitialization method of variable pricer (called before transformed problem is freed) */
#if 0
static
SCIP_DECL_PRICEREXIT(pricerExitDummy)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of dummy variable pricer not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define pricerExitDummy NULL
#endif


/** solving process initialization method of variable pricer (called when branch and bound process is about to begin) */
#if 0
static
SCIP_DECL_PRICERINITSOL(pricerInitsolDummy)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of dummy variable pricer not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define pricerInitsolDummy NULL
#endif


/** solving process deinitialization method of variable pricer (called before branch and bound process data is freed) */
#if 0
static
SCIP_DECL_PRICEREXITSOL(pricerExitsolDummy)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of dummy variable pricer not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define pricerExitsolDummy NULL
#endif


/** reduced cost pricing method of variable pricer for feasible LPs */
static
SCIP_DECL_PRICERREDCOST(pricerRedcostDummy)
{  /*lint --e{715}*/


   (*result) = SCIP_SUCCESS;

   return SCIP_OKAY;
}


/** Farkas pricing method of variable pricer for infeasible LPs */
static
SCIP_DECL_PRICERFARKAS(pricerFarkasDummy)
{  /*lint --e{715}*/

   (*result) = SCIP_SUCCESS;

   return SCIP_OKAY;
}





/*
 * variable pricer specific interface methods
 */

/** creates the dummy variable pricer and includes it in SCIP */
SCIP_RETCODE SCIPincludePricerDummy(
   SCIP*                 scip                /**< SCIP data structure */
   )
{
   SCIP_PRICERDATA* pricerdata;
   SCIP_PRICER* pricer;

   /* create dummy variable pricer data */
   pricerdata = NULL;
   /* TODO: (optional) create variable pricer specific data here */

   pricer = NULL;

   /* include variable pricer */

   /* use SCIPincludePricerBasic() plus setter functions if you want to set callbacks one-by-one and your code should
    * compile independent of new callbacks being added in future SCIP versions
    */
   SCIP_CALL( SCIPincludePricerBasic(scip, &pricer, PRICER_NAME, PRICER_DESC, PRICER_PRIORITY, PRICER_DELAY,
         pricerRedcostDummy, pricerFarkasDummy, pricerdata) );
   assert(pricer != NULL);

   /* set non fundamental callbacks via setter functions */
   /* SCIP_CALL( SCIPsetPricerCopy(scip, pricer, pricerCopyDummy) ); */
   /* SCIP_CALL( SCIPsetPricerFree(scip, pricer, pricerFreeDummy) ); */
   /* SCIP_CALL( SCIPsetPricerInit(scip, pricer, pricerInitDummy) ); */
   /* SCIP_CALL( SCIPsetPricerExit(scip, pricer, pricerExitDummy) ); */
   /* SCIP_CALL( SCIPsetPricerInitsol(scip, pricer, pricerInitsolDummy) ); */
   /* SCIP_CALL( SCIPsetPricerExitsol(scip, pricer, pricerExitsolDummy) ); */


   /* add dummy variable pricer parameters */
   /* TODO: (optional) add variable pricer specific parameters with SCIPaddTypeParam() here */

   return SCIP_OKAY;
}
