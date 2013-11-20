module Database.HyperDex.Internal.ReturnCode
  ( ReturnCode (..),
    AdminReturnCode (..)
  )
  where

#include "hyperdex/client.h"
#include "hyperdex/admin.h"

{#enum hyperdex_client_returncode as ReturnCode {underscoreToCase} deriving (Eq, Show) #}
{#enum hyperdex_admin_returncode as AdminReturnCode {underscoreToCase} deriving (Eq, Show) #}
