{-# LANGUAGE ViewPatterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HyperDex.Client
-- Copyright   :  (c) Aaron Friel 2013
-- License     :  BSD-style
-- Maintainer  :  mayreply@aaronfriel.com
-- Stability   :  maybe
-- Portability :  portable
--
-- A client connection based API for the HyperDex key value store.
--
-- This module exposes functions that rely on manually providing a client
-- parameter, and no guarantees are provided that resources will be cleaned
-- up unless the end-user ensures the connection is closed.
--
-----------------------------------------------------------------------------

module Database.HyperDex.Admin
  ( module Database.HyperDex.Internal.Admin
  -- Data structures
  , module Database.HyperDex.Internal.ReturnCode
  )
  where

import           Database.HyperDex.Internal.Admin
import           Database.HyperDex.Internal.ReturnCode
