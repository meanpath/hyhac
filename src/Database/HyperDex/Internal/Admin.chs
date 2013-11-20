{-# LANGUAGE ViewPatterns #-}

module Database.HyperDex.Internal.Admin
  ( HyperdexAdmin, connect
  )
  where

import Foreign
import Foreign.C

import Data.ByteString (ByteString)

{# import Database.HyperDex.Internal.ReturnCode #}
import Database.HyperDex.Internal.Util
-- FIX should be abstract
import Database.HyperDex.Internal.Client (connectHost, ConnectInfo, connectPort)

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Concurrent (yield, threadDelay)
import Control.Concurrent.MVar

import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as Text (pack)

import Data.Default



#include "hyperdex/admin.h"

{#pointer *hyperdex_admin as HyperdexAdmin #}-- | Connect to a HyperDex cluster.
hyperdex_adminCreate :: ByteString -> Word16 -> IO HyperdexAdmin
hyperdex_adminCreate h port = withCBString h $ \host ->
  wrapHyperCall $ {# call hyperdex_admin_create #} host (fromIntegral port)

connect :: ConnectInfo -> IO HyperdexAdmin
connect info = hyperdex_adminCreate (encodeUtf8 . Text.pack . connectHost $ info) (connectPort info)
