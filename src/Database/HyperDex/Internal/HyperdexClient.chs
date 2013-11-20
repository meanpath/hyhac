module Database.HyperDex.Internal.HyperdexClient
  ( -- Simple, single-key operations
    get
  , put
  , putIfNotExist
  , delete
  , putConditional
    -- Atomic numeric operations
  , atomicAdd, atomicSub
  , atomicMul, atomicDiv
    -- Atomic integral operations
  , atomicMod
  , atomicAnd, atomicOr
  , atomicXor
    -- Atomic string operations
  , atomicStringPrepend, atomicStringAppend
  , atomicListLPush, atomicListRPush
  , atomicSetAdd, atomicSetRemove
  , atomicSetUnion, atomicSetIntersect
    -- Atomic simple map operations
  , atomicMapInsert, atomicMapDelete
  -- , atomicConditionalMapInsert
    -- Atomic numeric value operations
  , atomicMapAdd, atomicMapSub
  , atomicMapMul, atomicMapDiv
    -- Atomic integral value operations
  , atomicMapMod
  , atomicMapAnd, atomicMapOr
  , atomicMapXor
    -- Atomic string value operations
  , atomicMapStringPrepend
  , atomicMapStringAppend
  -- Search operations
  , search
  , describeSearch
  , deleteGroup
  , count
  )
  where

import Foreign
import Foreign.C

import Data.ByteString (ByteString)
import Data.Text (Text)

#include "hyperdex/client.h"

{# import Database.HyperDex.Internal.ReturnCode #}
{# import Database.HyperDex.Internal.Client #}
{# import Database.HyperDex.Internal.Attribute #}
{# import Database.HyperDex.Internal.AttributeCheck #}
{# import Database.HyperDex.Internal.MapAttribute #}
import Database.HyperDex.Internal.Util

data Op = OpPut
        | OpPutIfNotExist
        | OpAtomicAdd
        | OpAtomicSub
        | OpAtomicMul
        | OpAtomicDiv
        | OpAtomicMod
        | OpAtomicAnd
        | OpAtomicOr
        | OpAtomicXor
        | OpAtomicStringPrepend
        | OpAtomicStringAppend
        | OpAtomicListLPush
        | OpAtomicListRPush
        | OpAtomicSetAdd
        | OpAtomicSetRemove
        | OpAtomicSetIntersect
        | OpAtomicSetUnion

data MapOp = OpAtomicMapInsert
           | OpAtomicMapDelete
           | OpAtomicMapAdd
           | OpAtomicMapSub
           | OpAtomicMapMul
           | OpAtomicMapDiv
           | OpAtomicMapMod
           | OpAtomicMapAnd
           | OpAtomicMapOr
           | OpAtomicMapXor
           | OpAtomicMapStringPrepend
           | OpAtomicMapStringAppend

put :: Client -> Text -> ByteString -> [Attribute] -> AsyncResult ()
put = hyperdex_clientOp OpPut

putIfNotExist :: Client -> Text -> ByteString -> [Attribute] -> AsyncResult ()
putIfNotExist = hyperdex_clientOp OpPutIfNotExist

atomicAdd, atomicSub,
  atomicMul, atomicDiv,
  atomicMod,
  atomicAnd, atomicOr,
  atomicXor,
  atomicStringPrepend, atomicStringAppend,
  atomicListLPush, atomicListRPush,
  atomicSetAdd, atomicSetRemove,
  atomicSetUnion, atomicSetIntersect :: Client -> Text -> ByteString -> [Attribute] -> AsyncResult ()

atomicAdd          = hyperdex_clientOp OpAtomicAdd
atomicSub          = hyperdex_clientOp OpAtomicSub
atomicMul          = hyperdex_clientOp OpAtomicMul
atomicDiv          = hyperdex_clientOp OpAtomicDiv
atomicMod          = hyperdex_clientOp OpAtomicMod
atomicAnd          = hyperdex_clientOp OpAtomicAnd
atomicOr           = hyperdex_clientOp OpAtomicOr
atomicXor          = hyperdex_clientOp OpAtomicXor
atomicStringPrepend      = hyperdex_clientOp OpAtomicStringPrepend
atomicStringAppend       = hyperdex_clientOp OpAtomicStringAppend
atomicListLPush    = hyperdex_clientOp OpAtomicListLPush
atomicListRPush    = hyperdex_clientOp OpAtomicListRPush
atomicSetAdd       = hyperdex_clientOp OpAtomicSetAdd
atomicSetRemove    = hyperdex_clientOp OpAtomicSetRemove
atomicSetIntersect = hyperdex_clientOp OpAtomicSetIntersect
atomicSetUnion     = hyperdex_clientOp OpAtomicSetUnion

atomicMapInsert,
  atomicMapDelete,
  atomicMapAdd,
  atomicMapSub,
  atomicMapMul,
  atomicMapDiv,
  atomicMapMod,
  atomicMapAnd,
  atomicMapOr,
  atomicMapXor,
  atomicMapStringPrepend,
  atomicMapStringAppend :: Client -> Text -> ByteString -> [MapAttribute] -> AsyncResult ()

atomicMapInsert = hyperdex_clientMapOp OpAtomicMapInsert
atomicMapDelete = hyperdex_clientMapOp OpAtomicMapDelete
atomicMapAdd    = hyperdex_clientMapOp OpAtomicMapAdd
atomicMapSub    = hyperdex_clientMapOp OpAtomicMapSub
atomicMapMul    = hyperdex_clientMapOp OpAtomicMapMul
atomicMapDiv    = hyperdex_clientMapOp OpAtomicMapDiv
atomicMapMod    = hyperdex_clientMapOp OpAtomicMapMod
atomicMapAnd    = hyperdex_clientMapOp OpAtomicMapAnd
atomicMapOr     = hyperdex_clientMapOp OpAtomicMapOr
atomicMapXor    = hyperdex_clientMapOp OpAtomicMapXor
atomicMapStringPrepend = hyperdex_clientMapOp OpAtomicMapStringPrepend
atomicMapStringAppend  = hyperdex_clientMapOp OpAtomicMapStringAppend

-- int64_t
-- hyperdex_client_put(struct hyperdex_client* client, const char* space, const char* key,
--                 size_t key_sz, const struct hyperdex_client_attribute* attrs,
--                 size_t attrs_sz, enum hyperdex_client_returncode* status);
get :: Client
    -> Text
    -> ByteString
    -> AsyncResult [Attribute]
get client s k = withClient client $ \hyperdex_client -> do
  returnCodePtr <- new (fromIntegral . fromEnum $ HyperdexClientGarbage)
  attributePtrPtr <- malloc
  attributeSizePtr <- malloc
  space <- newTextUtf8 s
  (key,keySize) <- newCBStringLen k
  handle <- wrapHyperCall $
            {# call hyperdex_client_get #}
              hyperdex_client
              space key (fromIntegral keySize)
              returnCodePtr attributePtrPtr attributeSizePtr
  let continuation = do
        returnCode <- fmap (toEnum . fromIntegral) $ peek returnCodePtr
        attributes <-
          case returnCode of
            HyperdexClientSuccess -> do
              attributePtr <- peek attributePtrPtr
              attributeSize <- fmap fromIntegral $ peek attributeSizePtr
              attrs <- peekArray attributeSize attributePtr
              hyperdexFreeAttributes attributePtr attributeSize
              return attrs
            _ -> return []
        free returnCodePtr
        free attributePtrPtr
        free attributeSizePtr
        free space
        free key
        return $
          case returnCode of
            HyperdexClientSuccess -> Right attributes
            _                  -> Left returnCode
  return (handle, continuation)

-- int64_t
-- hyperdex_client_del(struct hyperdex_client* client, const char* space, const char* key,
--                 size_t key_sz, enum hyperdex_client_returncode* status);
delete :: Client
       -> Text
       -> ByteString
       -> AsyncResult ()
delete client s k = withClient client $ \hyperdex_client -> do
  returnCodePtr <- new (fromIntegral . fromEnum $ HyperdexClientGarbage)
  space <- newTextUtf8 s
  (key,keySize) <- newCBStringLen k
  handle <- wrapHyperCall $
            {# call hyperdex_client_del #}
              hyperdex_client
              space key (fromIntegral keySize)
              returnCodePtr
  let continuation = do
        returnCode <- fmap (toEnum . fromIntegral) $ peek returnCodePtr
        free returnCodePtr
        free space
        free key
        return $
          case returnCode of
            HyperdexClientSuccess -> Right ()
            _                  -> Left returnCode
  return (handle, continuation)

-- int64_t
-- hyperdex_client_cond_put(struct hyperdex_client* client, const char* space,
--                      const char* key, size_t key_sz,
--                      const struct hyperdex_client_attribute_check* checks, size_t checks_sz,
--                      const struct hyperdex_client_attribute* attrs, size_t attrs_sz,
--                      enum hyperdex_client_returncode* status);
putConditional :: Client
               -> Text
               -> ByteString
               -> [AttributeCheck]
               -> [Attribute]
               -> AsyncResult ()
putConditional client s k checks attributes = withClient client $ \hyperdex_client -> do
  returnCodePtr <- new (fromIntegral . fromEnum $ HyperdexClientGarbage)
  space <- newTextUtf8 s
  (key,keySize) <- newCBStringLen k
  (attributePtr, attributeSize) <- newHyperDexAttributeArray attributes
  (checkPtr, checkSize) <- newHyperDexAttributeCheckArray checks
  handle <- wrapHyperCall $
            {# call hyperdex_client_cond_put #}
              hyperdex_client
              space key (fromIntegral keySize)
              checkPtr (fromIntegral checkSize)
              attributePtr (fromIntegral attributeSize)
              returnCodePtr
  let continuation = do
        returnCode <- fmap (toEnum . fromIntegral) $ peek returnCodePtr
        free returnCodePtr
        free space
        free key
        haskellFreeAttributes attributePtr attributeSize
        haskellFreeAttributeChecks checkPtr checkSize
        return $
          case returnCode of
            HyperdexClientSuccess -> Right ()
            _                  -> Left returnCode
  return (handle, continuation)

-- int64_t
-- hyperdex_client_atomic_xor(struct hyperdex_client* client, const char* space,
--                        const char* key, size_t key_sz,
--                        const struct hyperdex_client_attribute* attrs, size_t attrs_sz,
--                        enum hyperdex_client_returncode* status);
hyperdex_clientOp :: Op
              -> Client
              -> Text
              -> ByteString
              -> [Attribute]
              -> AsyncResult ()
hyperdex_clientOp op =
  \client s k attributes ->
    withClient client $ \hyperdex_client -> do
  returnCodePtr <- new (fromIntegral . fromEnum $ HyperdexClientGarbage)
  space <- newTextUtf8 s
  (key,keySize) <- newCBStringLen k
  (attributePtr, attributeSize) <- newHyperDexAttributeArray attributes
  let ccall = case op of
              OpPut           -> {# call hyperdex_client_put #}
              OpPutIfNotExist -> {# call hyperdex_client_put_if_not_exist #}
              OpAtomicAdd     -> {# call hyperdex_client_atomic_add #}
              OpAtomicSub     -> {# call hyperdex_client_atomic_sub #}
              OpAtomicMul     -> {# call hyperdex_client_atomic_mul #}
              OpAtomicDiv     -> {# call hyperdex_client_atomic_div #}
              OpAtomicMod     -> {# call hyperdex_client_atomic_mod #}
              OpAtomicAnd     -> {# call hyperdex_client_atomic_and #}
              OpAtomicOr      -> {# call hyperdex_client_atomic_or  #}
              OpAtomicXor     -> {# call hyperdex_client_atomic_xor #}
              OpAtomicStringPrepend -> {# call hyperdex_client_string_prepend #}
              OpAtomicStringAppend  -> {# call hyperdex_client_string_append  #}
              OpAtomicListLPush     -> {# call hyperdex_client_list_lpush #}
              OpAtomicListRPush     -> {# call hyperdex_client_list_rpush #}
              OpAtomicSetAdd        -> {# call hyperdex_client_set_add       #}
              OpAtomicSetRemove     -> {# call hyperdex_client_set_remove    #}
              OpAtomicSetIntersect  -> {# call hyperdex_client_set_intersect #}
              OpAtomicSetUnion      -> {# call hyperdex_client_set_union     #}
  handle <- wrapHyperCall $
            ccall
              hyperdex_client
              space key (fromIntegral keySize)
              attributePtr (fromIntegral attributeSize) returnCodePtr
  let continuation = do
        returnCode <- fmap (toEnum . fromIntegral) $ peek returnCodePtr
        free returnCodePtr
        free space
        free key
        hyperdexFreeAttributes attributePtr attributeSize
        return $
          case returnCode of
            HyperdexClientSuccess -> Right ()
            _                  -> Left returnCode
  return (handle, continuation)
{-# INLINE hyperdex_clientOp #-}

-- int64_t
-- hyperdex_client_map_add(struct hyperdex_client* client, const char* space,
--                     const char* key, size_t key_sz,
--                     const struct hyperdex_client_map_attribute* attrs, size_t attrs_sz,
--                     enum hyperdex_client_returncode* status);
hyperdex_clientMapOp :: MapOp
                 -> Client
                 -> Text
                 -> ByteString
                 -> [MapAttribute]
                 -> AsyncResult ()
hyperdex_clientMapOp op =
  \client s k mapAttributes ->
    withClient client $ \hyperdex_client -> do
  returnCodePtr <- new (fromIntegral . fromEnum $ HyperdexClientGarbage)
  space <- newTextUtf8 s
  (key,keySize) <- newCBStringLen k
  (mapAttributePtr, mapAttributeSize) <- newHyperDexMapAttributeArray mapAttributes
  let ccall = case op of
              OpAtomicMapInsert -> {# call hyperdex_client_map_add    #}
              OpAtomicMapDelete -> error "not implemented" -- {# call hyperdex_client_map_remove #}
              OpAtomicMapAdd    -> {# call hyperdex_client_map_atomic_add #}
              OpAtomicMapSub    -> {# call hyperdex_client_map_atomic_sub #}
              OpAtomicMapMul    -> {# call hyperdex_client_map_atomic_mul #}
              OpAtomicMapDiv    -> {# call hyperdex_client_map_atomic_div #}
              OpAtomicMapMod    -> {# call hyperdex_client_map_atomic_mod #}
              OpAtomicMapAnd    -> {# call hyperdex_client_map_atomic_and #}
              OpAtomicMapOr     -> {# call hyperdex_client_map_atomic_or  #}
              OpAtomicMapXor    -> {# call hyperdex_client_map_atomic_xor #}
              OpAtomicMapStringPrepend -> {# call hyperdex_client_map_string_prepend #}
              OpAtomicMapStringAppend  -> {# call hyperdex_client_map_string_append  #}
  handle <- wrapHyperCall $
            ccall
              hyperdex_client space
              key (fromIntegral keySize)
              mapAttributePtr (fromIntegral mapAttributeSize)
              returnCodePtr
  let continuation = do
        returnCode <- fmap (toEnum . fromIntegral) $ peek returnCodePtr
        free returnCodePtr
        free space
        free key
        --haskellFreeMapAttributes mapAttributePtr mapAttributeSize
        return $
          case returnCode of
            HyperdexClientSuccess -> Right ()
            _                  -> Left returnCode
  return (handle, continuation)
{-# INLINE hyperdex_clientMapOp #-}

-- int64_t
-- hyperdex_client_map_add(struct hyperdex_client* client, const char* space,
--                     const char* key, size_t key_sz,
--                     const struct hyperdex_client_map_attribute* attrs, size_t attrs_sz,
--                     enum hyperdex_client_returncode* status);
---- A compilation error prevents this from being implemented:
-- /home/cloudium/git/hyhac/dist/build/libHShyhac-0.2.0.0_p.a(HyperdexClient.p_o): In function `ra9O_info':
-- /tmp/ghc20379_0/ghc20379_1.p_o:(.text+0x1d6): undefined reference to `hyperdex_client_cond_map_add'
-- /home/cloudium/git/hyhac/dist/build/libHShyhac-0.2.0.0_p.a(HyperdexClient.p_o): In function `sdlx_info':
-- /tmp/ghc20379_0/ghc20379_1.p_o:(.text+0x3d08d): undefined reference to `hyperdex_client_cond_map_add'
-- /home/cloudium/git/hyhac/dist/build/libHShyhac-0.2.0.0_p.a(HyperdexClient.p_o): In function `sdm2_info':
-- /tmp/ghc20379_0/ghc20379_1.p_o:(.text+0x3dc85): undefined reference to `hyperdex_client_cond_map_add'

-- atomicConditionalMapInsert :: HyperdexClient -> Text -> ByteString
--                            -> [AttributeCheck]
--                            -> [MapAttribute]
--                            -> AsyncResultHandle ()
-- atomicConditionalMapInsert client s k checks mapAttributes = do
--   returnCodePtr <- new (fromIntegral . fromEnum $ HyperdexClientGarbage)
--   space <- newTextUtf8 s
--   (key,keySize) <- newCBStringLen k
--   (checkPtr, checkSize) <- newHyperDexAttributeCheckArray checks
--   (mapAttributePtr, mapAttributeSize) <- newHyperDexMapAttributeArray mapAttributes
--   handle <- {# call hyperdex_client_cond_map_add #}
--               client space
--               key (fromIntegral keySize)
--               checkPtr (fromIntegral checkSize)
--               mapAttributePtr (fromIntegral mapAttributeSize)
--               returnCodePtr
--   let continuation = do
--         returnCode <- fmap (toEnum . fromIntegral) $ peek returnCodePtr
--         free returnCodePtr
--         free space
--         free key
--         --haskellFreeMapAttributes mapAttributePtr mapAttributeSize
--         return $
--           case returnCode of
--             HyperdexClientSuccess -> Right ()
--             _                  -> Left returnCode
--   return (handle, continuation)
-- {-# INLINE atomicConditionalMapInsert #-}

-- int64_t
-- hyperdex_client_search(struct hyperdex_client* client, const char* space,
--                    const struct hyperdex_client_attribute_check* checks, size_t checks_sz,
--                    enum hyperdex_client_returncode* status,
--                    struct hyperdex_client_attribute** attrs, size_t* attrs_sz);
search :: Client
          -> Text
          -> [AttributeCheck]
          -> AsyncResult (SearchStream [Attribute])
search client s checks = withClientStream client $ \hyperdex_client -> do
  returnCodePtr <- new (fromIntegral . fromEnum $ HyperdexClientGarbage)
  space <- newTextUtf8 s
  (checkPtr, checkSize) <- newHyperDexAttributeCheckArray checks
  resultSetPtrPtr <- malloc
  resultSetSizePtr <- malloc
  handle <- wrapHyperCall $
            {# call hyperdex_client_search #}
              hyperdex_client space
              checkPtr (fromIntegral checkSize :: {# type size_t #})
              returnCodePtr
              resultSetPtrPtr resultSetSizePtr
  case handle >= 0 of
    True -> do
      let continuation (Just HyperdexClientSuccess) = do
            returnCode <- fmap (toEnum . fromIntegral) $ peek returnCodePtr
            case returnCode of
              HyperdexClientSuccess -> do
                resultSize <- peek resultSetSizePtr
                resultSetPtr <- peek resultSetPtrPtr
                resultSet <- peekArray (fromIntegral resultSize) resultSetPtr
                return $ Right resultSet
              _ -> return $ Left returnCode
          continuation e = do
            free returnCodePtr
            free space
            haskellFreeAttributeChecks checkPtr checkSize
            free resultSetPtrPtr
            free resultSetSizePtr
            return $ Left $ case e of
                              Nothing -> HyperdexClientDupeattr
                              Just rc -> rc
      return (handle, continuation)
    False -> do
      let continuation = const $ do
            returnCode <- fmap (toEnum . fromIntegral) $ peek returnCodePtr
            free returnCodePtr
            free space
            haskellFreeAttributeChecks checkPtr checkSize
            free resultSetPtrPtr
            free resultSetSizePtr
            return $ Left returnCode
      return (handle, continuation)

-- int64_t
-- hyperdex_client_group_del(struct hyperdex_client* client, const char* space,
--                       const struct hyperdex_client_attribute_check* checks, size_t checks_sz,
--                       enum hyperdex_client_returncode* status);
--
deleteGroup :: Client
            -> Text
            -> [AttributeCheck]
            -> AsyncResult ()
deleteGroup client s checks = withClient client $ \hyperdex_client -> do
  returnCodePtr <- new (fromIntegral . fromEnum $ HyperdexClientGarbage)
  space <- newTextUtf8 s
  (checkPtr, checkSize) <- newHyperDexAttributeCheckArray checks
  handle <- wrapHyperCall $
            {# call hyperdex_client_group_del #}
              hyperdex_client space
              checkPtr (fromIntegral checkSize)
              returnCodePtr
  let continuation = do
        returnCode <- fmap (toEnum . fromIntegral) $ peek returnCodePtr
        free returnCodePtr
        free space
        haskellFreeAttributeChecks checkPtr checkSize
        return $
          case returnCode of
            HyperdexClientSuccess -> Right ()
            _                  -> Left returnCode
  return (handle, continuation)

-- int64_t
-- hyperdex_client_search_describe(struct hyperdex_client* client, const char* space,
--                             const struct hyperdex_client_attribute_check* checks, size_t checks_sz,
--                             enum hyperdex_client_returncode* status, const char** description);
describeSearch :: Client
               -> Text
               -> [AttributeCheck]
               -> AsyncResult Text
describeSearch client s checks = withClient client $ \hyperdex_client -> do
  returnCodePtr <- new (fromIntegral . fromEnum $ HyperdexClientGarbage)
  space <- newTextUtf8 s
  (checkPtr, checkSize) <- newHyperDexAttributeCheckArray checks
  descPtr <- malloc
  handle <- wrapHyperCall $
            {# call hyperdex_client_search_describe #}
              hyperdex_client space
              checkPtr (fromIntegral checkSize)
              returnCodePtr descPtr
  let continuation = do
        returnCode <- fmap (toEnum . fromIntegral) $ peek returnCodePtr
        free returnCodePtr
        free space
        haskellFreeAttributeChecks checkPtr checkSize
        desc <- peekTextUtf8 descPtr
        return $
          case returnCode of
            HyperdexClientSuccess -> Right desc
            _                  -> Left returnCode
  return (handle, continuation)

-- int64_t
-- hyperdex_client_count(struct hyperdex_client* client, const char* space,
--                   const struct hyperdex_client_attribute_check* checks, size_t checks_sz,
--                   enum hyperdex_client_returncode* status, uint64_t* result);
count :: Client
      -> Text
      -> [AttributeCheck]
      -> AsyncResult Integer
count client s checks = withClient client $ \hyperdex_client -> do
  returnCodePtr <- new (fromIntegral . fromEnum $ HyperdexClientGarbage)
  space <- newTextUtf8 s
  (checkPtr, checkSize) <- newHyperDexAttributeCheckArray checks
  countPtr <- new 1010101010
  handle <- wrapHyperCall $
            {# call hyperdex_client_count #}
              hyperdex_client space
              checkPtr (fromIntegral checkSize)
              returnCodePtr countPtr
  let continuation = do
        returnCode <- fmap (toEnum . fromIntegral) $ peek returnCodePtr
        free returnCodePtr
        free space
        haskellFreeAttributeChecks checkPtr checkSize
        n <- fmap fromIntegral $ peek countPtr
        return $
          -- TODO: Why does this succeed even when it fails?
          -- returnCode of HyperdexClientGarbage (what we put in returnCodePtr)
          -- usually means success, but occasionally failure.
          -- Is this a bug in hyhac or HyperDex?
          case returnCode of
            HyperdexClientSuccess -> Right n
            HyperdexClientGarbage -> Right n
            _                  -> Left returnCode
  return (handle, continuation)
