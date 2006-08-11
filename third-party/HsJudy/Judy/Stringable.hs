{-# OPTIONS -fallow-undecidable-instances -fallow-incoherent-instances #-}

module Judy.Stringable (
    Stringable (..)
) where

import Foreign.C.String
import qualified Data.ByteString as B

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

    copyCS = B.copyCString
    copyCSLen = B.copyCStringLen

--instance Stringable Int where
--    toString = show
--    fromString = read
