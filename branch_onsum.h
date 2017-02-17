/**@file   branch_onsum.h
 * @brief  onsum branching rule
 * @author James Cussens
 */

/*---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2*/

#ifndef __SCIP_BRANCH_ONSUM_H__
#define __SCIP_BRANCH_ONSUM_H__


#include "scip/scip.h"

#ifdef __cplusplus
extern "C" {
#endif

/** creates the onsum branching rule and includes it in SCIP */
EXTERN
SCIP_RETCODE SCIPincludeBranchruleOnsum(
   SCIP*                 scip               /**< SCIP data structure */
   );

#ifdef __cplusplus
}
#endif

#endif
