{-# OPTIONS -fallow-undecidable-instances -fallow-incoherent-instances #-}

module Judy.Stringable (
    Stringable (..)
) where

import Foreign.C.String

-- #if __GLASGOW_HASKELL__ >= 605
-- import qualified Data.ByteString as B
-- #endif

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

-- FIXME: use pugs-fps

-- #if __GLASGOW_HASKELL__ >= 605
-- instance Stringable B.ByteString where
--     toString = undefined
--     fromString = undefined
-- 
--     useAsCS = B.useAsCString 
--     useAsCSLen = B.useAsCStringLen
-- 
--     copyCS = B.copyCString
--     copyCSLen = B.copyCStringLen
-- #endif


--instance Stringable Int where
--    toString = show
--    fromString = read
