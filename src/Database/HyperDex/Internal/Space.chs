module Database.HyperDex.Internal.Space
  (addSpace, removeSpace)
  where

import Foreign
import Foreign.C

import Data.Text (Text)

{#import Database.HyperDex.Internal.Client #}
{#import Database.HyperDex.Internal.Admin #}
{#import Database.HyperDex.Internal.ReturnCode #}
import Database.HyperDex.Internal.Util
import Control.Applicative

#include "hyperdex/client.h"
#include "hyperdex/admin.h"

-- addSpace :: HyperdexAdmin -> Text -> IO ReturnCode
-- addSpace c desc  = withClientImmediate c $ \hc -> do
--   hyperdex_adminAddSpace hc desc

-- removeSpace :: HyperdexAdmin -> Text -> IO ReturnCode
-- removeSpace c name = withClientImmediate c $ \hc -> do
--   hyperdex_adminRemoveSpace hc name

addSpace = hyperdex_adminAddSpace
removeSpace = hyperdex_adminRemoveSpace

-- enum hyperdex_client_returncode
--hyperdex_admin_add_space(struct hyperdex_client* client, const char* description);
hyperdex_adminAddSpace :: HyperdexAdmin -> Text -> IO AdminReturnCode
hyperdex_adminAddSpace = adminTask {#call hyperdex_admin_add_space #}

-- enum hyperdex_client_returncode
-- hyperdex_admin_rm_space(struct hyperdex_client* client, const char* space);
hyperdex_adminRemoveSpace :: HyperdexAdmin -> Text -> IO AdminReturnCode
hyperdex_adminRemoveSpace = adminTask {#call hyperdex_admin_rm_space #}


-- data Hole = Hole

-- foo :: Hole
-- foo = {#call hyperdex_admin_rm_space #}
