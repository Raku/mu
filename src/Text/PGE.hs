{-# OPTIONS_GHC -fglasgow-exts -fvia-C #-}
{-# OPTIONS_GHC -#include "pge.h" #-}

module Text.PGE (
    parseRule,
    pge_gen, pge_parse_new, pge_parse_free,
    Node(..), NodeType(..),
) where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array    

parseRule :: String -> IO String
parseRule str = withCString str $ \cstr -> do
    pge_init
    parsed  <- pge_parse cstr
    node    <- readNode parsed
    pge_parse_free parsed
    return $ show node

data NodeType
    = TNull
    | TPatternEnd
    | TDot
    | TLiteral
    | TConcat
    | TGroup
    | TAlt
    | TAnchorBos
    | TAnchorEos
    | TAnchorBol
    | TAnchorEol
    | TCutAlt
    | TCutRule
    | TBackreference
    deriving (Ord, Eq, Show, Enum)

data Node = MkNode
    { nodeId    :: Int
    , nodeType  :: NodeType
    , nodeNLen  :: Int
    , nodeMin   :: Int
    , nodeMax   :: Int
    , isGreedy  :: Bool
    , isCut     :: Bool
    , nodeGroup :: Int
    , nodeName  :: String
    , nodeLeft  :: Maybe Node
    , nodeRight :: Maybe Node
    }
    deriving (Ord, Eq, Show);

type PgeExp = Ptr CInt

readNode :: PgeExp -> IO (Maybe Node)
readNode exp | exp == nullPtr = return Nothing
readNode exp = do
    [id, typ, nlen, min, max, isgreedy, iscut, group] <- peekArray 8 exp
    let ptr = (castPtr $ advancePtr exp 8)
    [namePtr, leftPtr, rightPtr] <- peekArray 3 ptr
    name    <- if namePtr == nullPtr then return "" else peekCString namePtr
    left    <- readNode $ castPtr leftPtr
    right   <- readNode $ castPtr rightPtr
    return $ Just MkNode
        { nodeId    = fromEnum id
        , nodeType  = (toEnum . fromEnum) typ
        , nodeNLen  = fromEnum nlen
        , nodeMin   = fromEnum min
        , nodeMax   = fromEnum max
        , isGreedy  = (isgreedy == 0)
        , isCut     = (iscut == 0)
        , nodeGroup = fromEnum group
        , nodeName  = name
        , nodeLeft  = left
        , nodeRight = right
        }

foreign import ccall
    pge_init :: IO ()

foreign import ccall
    pge_gen :: PgeExp -> IO CString

foreign import ccall
    pge_parse :: CString -> IO PgeExp

foreign import ccall
    pge_parse_new :: CInt -> PgeExp -> PgeExp -> IO PgeExp

foreign import ccall
    pge_parse_free :: PgeExp -> IO ()

{-
foreign import ccall
    pge_p6rule_pir :: CString -> IO CString
-}
