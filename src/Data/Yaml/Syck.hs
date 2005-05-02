{-# OPTIONS_GHC -fglasgow-exts -fvia-C #-}
{-# OPTIONS_GHC -#include "syck.h" #-}

module Data.Yaml.Syck (
    parseYaml,
    YamlNode(..),
) where

import Control.Exception (bracket)
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Storable

data YamlNode
    = YamlMap [(YamlNode, YamlNode)]
    | YamlSeq [YamlNode]
    | YamlStr String
    deriving (Show, Ord, Eq)

type SYMID = CULong
type SyckNode = Ptr ()
type SyckParser = Ptr ()
type SyckNodeHandler = SyckParser -> SyckNode -> IO SYMID
type SyckErrorHandler = SyckParser -> CString -> IO ()
type SyckNodePtr = Ptr CString
data SyckKind = SyckMap | SyckSeq | SyckStr
    deriving (Show, Ord, Eq, Enum)

parseYaml :: String -> IO YamlNode
parseYaml str = withCString str $ \cstr -> do
    bracket syck_new_parser syck_free_parser $ \parser -> do
        syck_parser_str_auto parser cstr nullFunPtr
        syck_parser_handler parser =<< mkNodeCallback nodeCallback
        syck_parser_error_handler parser =<< mkErrorCallback errorCallback
        syck_parser_implicit_typing parser 1
        syck_parser_taguri_expansion parser 0
        symId   <- syck_parse parser
        readNode parser symId

nodeCallback :: SyckParser -> SyckNode -> IO SYMID
nodeCallback parser syckNode = do
    kind    <- syckNodeKind syckNode
    len     <- syckNodeLength kind syckNode
    node    <- parseNode kind parser syckNode len
    nodePtr <- freezeNode node
    symId   <- syck_add_sym parser nodePtr
    return (toEnum . fromEnum $ symId)

errorCallback :: SyckParser -> CString -> IO ()
errorCallback parser cstr = do
    msg     <- peekCString cstr
    lineptr <- peekByteOff parser offsetLinePtr :: IO CChar
    cursor  <- peekByteOff parser (offsetLinePtr + (3 * sizeOfCString)) :: IO CChar
    linect  <- peekByteOff parser (offsetLinePtr + (6 * sizeOfCString)) :: IO CChar
    fail $ concat
        [ msg
        , ": line ", show (1 + fromEnum linect)
        , ", column ", show (cursor - lineptr)
        ]

freezeNode :: YamlNode -> IO SyckNodePtr
freezeNode node = do
    ptr     <- newStablePtr node
    new (castPtr $ castStablePtrToPtr ptr)

readNode :: SyckParser -> SYMID -> IO YamlNode
readNode parser symId = alloca $ \nodePtr -> do
    syck_lookup_sym parser symId nodePtr
    ptr     <- peek . castPtr =<< peek nodePtr
    deRefStablePtr (castPtrToStablePtr ptr)

sizeOfSYMID         = sizeOf (undefined :: SYMID)
sizeOfCInt          = sizeOf (undefined :: CInt)
sizeOfCString       = sizeOf (undefined :: CString)
sizeOfCLong         = sizeOf (undefined :: CLong)
sizeOfCSize         = sizeOf (undefined :: CSize)
sizeOfFunPtr        = sizeOf (undefined :: FunPtr ())
sizeOfNodeHeader    = sizeOfSYMID + sizeOfCInt + (sizeOfCString * 2)

offsetLinePtr = sum
    [ sizeOfSYMID   * 2
    , sizeOfCInt    * 2
    , sizeOfFunPtr  * 3
    , sizeOfCInt    * 2
    , sizeOfCSize
    , sizeOfCString * 2
    ]

offsetLength :: SyckKind -> Int
offsetLength SyckMap = sizeOfCInt + (2 * sizeOfSYMID) + sizeOfCLong
offsetLength SyckSeq = sizeOfCInt + sizeOfSYMID + sizeOfCLong
offsetLength SyckStr = sizeOfCInt + sizeOfCString

syckNodeKind :: SyckNode -> IO SyckKind
syckNodeKind syckNode = do
    cint    <- peekByteOff syckNode sizeOfSYMID :: IO CInt
    return (toEnum . fromEnum $ cint)

syckNodeLength :: SyckKind -> SyckNode -> IO CLong
syckNodeLength kind syckNode = do
    ptr     <- peekByteOff syckNode sizeOfNodeHeader :: IO (Ptr ())
    peekByteOff ptr (offsetLength kind)

parseNode :: SyckKind -> SyckParser -> SyckNode -> CLong -> IO YamlNode
parseNode SyckMap parser syckNode len = do
    pairs <- (`mapM` [0..len-1]) $ \idx -> do
        keyId   <- syck_map_read syckNode 0 idx
        key     <- readNode parser keyId
        valId   <- syck_map_read syckNode 1 idx
        val     <- readNode parser valId
        return (key, val)
    return $ YamlMap pairs

parseNode SyckSeq parser syckNode len = do
    nodes <- (`mapM` [0..len-1]) $ \idx -> do
        symId   <- syck_seq_read syckNode idx
        readNode parser symId
    return $ YamlSeq nodes

parseNode SyckStr _ syckNode len = do
    cstr    <- syck_str_read syckNode
    str     <- peekCStringLen (cstr, fromEnum len)
    return $ YamlStr str

foreign import ccall "wrapper"  
    mkNodeCallback :: SyckNodeHandler -> IO (FunPtr SyckNodeHandler)

foreign import ccall "wrapper"  
    mkErrorCallback :: SyckErrorHandler -> IO (FunPtr SyckErrorHandler)

foreign import ccall
    syck_new_parser :: IO SyckParser

foreign import ccall
    syck_parser_str_auto :: SyckParser -> CString -> FunPtr () -> IO ()

foreign import ccall
    syck_parser_error_handler :: SyckParser -> FunPtr SyckErrorHandler -> IO ()

foreign import ccall
    syck_parser_implicit_typing :: SyckParser -> CInt -> IO ()

foreign import ccall
    syck_parser_taguri_expansion :: SyckParser -> CInt -> IO ()

foreign import ccall
    syck_parser_handler :: SyckParser -> FunPtr SyckNodeHandler -> IO ()

foreign import ccall
    syck_add_sym :: SyckParser -> SyckNodePtr -> IO CInt

foreign import ccall
    syck_parse :: SyckParser -> IO SYMID

foreign import ccall
    syck_free_parser :: SyckParser -> IO ()

foreign import ccall
    syck_lookup_sym :: SyckParser -> SYMID -> Ptr SyckNodePtr -> IO CInt

foreign import ccall
    syck_str_read :: SyckNode -> IO CString

foreign import ccall
    syck_seq_read :: SyckNode -> CLong -> IO SYMID

foreign import ccall
    syck_map_read :: SyckNode -> CInt -> CLong -> IO SYMID

