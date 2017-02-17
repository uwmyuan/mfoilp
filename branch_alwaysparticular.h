/**@file   branch_alwaysparticular.h
 * @brief  alwaysparticular branching rule
 * @author Tobias Achterberg
 */

/*---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2*/

#ifndef __SCIP_BRANCH_ALWAYSPARTICULAR_H__
#define __SCIP_BRANCH_ALWAYSPARTICULAR_H__


#include "scip/scip.h"

#ifdef __cplusplus
extern "C" {
#endif

/** creates the alwaysparticular branching rule and includes it in SCIP */
EXTERN
SCIP_RETCODE SCIPincludeBranchruleAlwaysparticular(
   SCIP*                 scip,               /**< SCIP data structure */
   SCIP_VAR*             var                 /**< variable to branch on (if not fixed ) */
   );

#ifdef __cplusplus
}
#endif

#endif
