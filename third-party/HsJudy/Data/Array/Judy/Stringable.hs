{-# LANGUAGE TypeSynonymInstances #-}

module Data.Array.Judy.Stringable (
    Stringable (..)
) where

import Foreign.C.String
import qualified Data.ByteString as B (ByteString, useAsCString, useAsCStringLen)
import Data.ByteString.Unsafe as BU (unsafePackCString, unsafePackCStringLen)
-- TODO: See if its possible to use Storable, ie. to let any Storable type be "stringable".

class Stringable k where
    toString :: k -> String
    fromString :: String -> k

    useAsCS :: k -> (CString -> IO a) -> IO a
    useAsCS k = withCAString (toString k)
    useAsCSLen :: k -> (CStringLen -> IO a) -> IO a
    useAsCSLen k = withCAStringLen (toString k)

    copyCS :: CString -> IO k
    copyCS c = peekCAString c >>= return . fromString
    copyCSLen :: CStringLen -> IO k
    copyCSLen c = peekCAStringLen c >>= return . fromString

instance Stringable String where
    toString = id
    fromString = id

instance Stringable B.ByteString where
    toString = undefined
    fromString = undefined

    useAsCS = B.useAsCString
    useAsCSLen = B.useAsCStringLen

    copyCS = BU.unsafePackCString
    copyCSLen = BU.unsafePackCStringLen

--instance Stringable Int where
--    toString = show
--    fromString = read
