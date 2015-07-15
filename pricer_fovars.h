/**@file   pricer_fovars.h
 * @ingroup PRICERS
 * @brief  fovars variable pricer
 * @author James Cussens
 */

/*---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2*/

#ifndef __SCIP_PRICER_FOVARS_H__
#define __SCIP_PRICER_FOVARS_H__


#include "scip/scip.h"

#ifdef __cplusplus
extern "C" {
#endif

/** creates the fovars variable pricer and includes it in SCIP */
EXTERN
SCIP_RETCODE SCIPincludePricerFovars(
   SCIP*                 scip                /**< SCIP data structure */
   );

#ifdef __cplusplus
}
#endif

#endif
