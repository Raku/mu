{-# OPTIONS_GHC -fglasgow-exts -fvia-C #-}
#include "../../syck/syck.h"
#include "../../cbits/fpstring.h"

module Data.Yaml.Syck (
    parseYaml, emitYaml,
    YamlNode(..), YamlElem(..), tagNode, nilNode, mkNode, mkTagNode,
) where

import Control.Exception (bracket)
import Data.IORef
import Data.Word                (Word8)
-- import qualified Data.FastPackedString as P
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.ForeignPtr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Storable
import Data.Maybe (isJust)
import Control.Monad (when)

import Debug.Trace
import Control.Monad.Trans

type YamlTag    = Maybe String
type YamlAnchor = Maybe String
type SYMID = CULong

data YamlNode = MkYamlNode
    { nid      :: SYMID
    , el       :: YamlElem
    , tag      :: YamlTag
    , anchor   :: YamlAnchor
    , shortcut :: (Maybe YamlNode)
    }
    deriving (Show, Ord, Eq)

data YamlElem
    = YamlMap [(YamlNode, YamlNode)]
    | YamlSeq [YamlNode]
    | YamlStr String
    | YamlNil
    deriving (Show, Ord, Eq)

type SyckNode = Ptr ()
type SyckParser = Ptr ()
type SyckNodeHandler = SyckParser -> SyckNode -> IO SYMID
type SyckErrorHandler = SyckParser -> CString -> IO ()
type SyckNodePtr = Ptr CString
type FSPtr = Ptr CString
type SyckEmitter = Ptr ()  
type SyckEmitterHandler = SyckEmitter -> Ptr () -> IO ()
type SyckOutputHandler = SyckEmitter -> CString -> CLong -> IO ()
data SyckKind = SyckMap | SyckSeq | SyckStr | SyckNil
    deriving (Show, Ord, Eq, Enum)

nilNode :: YamlNode
nilNode = MkYamlNode 0 YamlNil Nothing Nothing Nothing

tagNode :: YamlTag -> YamlNode -> YamlNode
tagNode _   MkYamlNode{tag=Just x} = error ("can't add tag: already tagged with" ++ x)
tagNode tag node                   = node{tag = tag}

mkNode :: YamlElem -> YamlNode
mkNode x = MkYamlNode 0 x Nothing Nothing Nothing

mkTagNode :: String -> YamlElem -> YamlNode
mkTagNode s x = MkYamlNode 0 x (Just s) Nothing Nothing

-- the extra commas here are not a bug
#enum CInt, , scalar_none, scalar_1quote, scalar_2quote, scalar_fold, scalar_literal, scalar_plain
#enum CInt, , seq_none, seq_inline
#enum CInt, , map_none, map_inline

#def typedef void* EmitterExtras;
type EmitterExtras = Ptr ()

emitYaml :: YamlNode -> IO (Either String String)
emitYaml node = do
    bracket syck_new_emitter syck_free_emitter $ \emitter -> do
        -- set up output port
        out    <- newIORef ""
        outPtr <- fmap castStablePtrToPtr (newStablePtr out)
        #{poke SyckEmitter, bonus} emitter outPtr
        #{poke SyckEmitter, style} emitter scalarFold
        syck_output_handler emitter =<< mkOutputCallback outputCallback
        syck_emitter_handler emitter =<< mkEmitterCallback emitterCallback
        nodePtr <- freezeNode node
        let nodePtr' = fromIntegral $ nodePtr `minusPtr` nullPtr
        syck_emit emitter nodePtr'
        syck_emitter_flush emitter 0
        fmap Right $ readIORef out

outputCallback :: SyckEmitter -> CString -> CLong -> IO ()
outputCallback emitter buf len = do
    outPtr  <- #{peek SyckEmitter, bonus} emitter
    out     <- deRefStablePtr (castPtrToStablePtr outPtr)
    str     <- peekCStringLen (buf, fromIntegral len)
    #{poke SyckEmitter, headless} emitter (1 :: CInt)
    modifyIORef out (++ str)

freezeFS :: ForeignPtr Word8 -> IO FSPtr
freezeFS ps = do
    ptr     <- newStablePtr ps
    return (castPtr $ castStablePtrToPtr ptr)

readFS :: FSPtr -> IO (ForeignPtr Word8)
readFS fs = do
    ptr     <- peek . castPtr =<< peek fs
    deRefStablePtr (castPtrToStablePtr ptr)

emitterCallback :: SyckEmitter -> Ptr () -> IO ()
emitterCallback e vp = emitNode e =<< thawNode vp
    
emitNode :: SyckEmitter -> YamlNode -> IO ()
emitNode e n@(MkYamlNode{el = YamlNil}) = do
    withTag n "string" $ \tag ->
        withCString "~" $ \cs ->       
            syck_emit_scalar e tag scalarNone 0 0 0 cs 1

emitNode e n@(MkYamlNode{el = YamlStr str}) = do
    withTag n "string" $ \tag ->       
        withCString str $ \cs ->       
            syck_emit_scalar e tag scalarNone 0 0 0 cs (toEnum $ length str)

emitNode e n@(MkYamlNode{el = YamlSeq seq}) = do
    withTag n "array" $ \tag ->
        syck_emit_seq e tag seqNone
    -- TODO: fix pesky warning about "integer from pointer without a cast" here
    mapM_ (syck_emit_item e) =<< (mapM freezeNode seq)
    syck_emit_end e

emitNode e n@(MkYamlNode{el = YamlMap m}) = do
    withTag n "hash" $ \tag -> 
        syck_emit_map e tag mapNone
    flip mapM_ m (\(k,v) -> do
        syck_emit_item e =<< freezeNode k
        syck_emit_item e =<< freezeNode v)
    syck_emit_end e

withTag :: YamlNode -> String -> (CString -> IO a) -> IO a
withTag node def f = withCString (maybe def id (tag node)) f

parseYaml :: String -> IO (Either String (Maybe YamlNode))
parseYaml str = withCString str $ \cstr -> do
    bracket syck_new_parser syck_free_parser $ \parser -> do
        err <- newIORef Nothing
        syck_parser_str_auto parser cstr nullFunPtr
        syck_parser_handler parser =<< mkNodeCallback nodeCallback
        syck_parser_error_handler parser =<< mkErrorCallback (errorCallback err)
        syck_parser_implicit_typing parser 1
        syck_parser_taguri_expansion parser 0
        symId <- syck_parse parser
        if symId /= 0 then fmap (Right . Just) (readNode parser symId) else do
        rv <- readIORef err
        return $ case rv of
            Nothing     -> Right Nothing
            Just err    -> Left err

nodeCallback :: SyckParser -> SyckNode -> IO SYMID
nodeCallback parser syckNode = do
    kind    <- syckNodeKind syckNode
    len     <- syckNodeLength kind syckNode
    node    <- parseNode kind parser syckNode len
    nodePtr <- writeNode node
    symId   <- syck_add_sym parser nodePtr
    return (toEnum . fromEnum $ symId)

errorCallback :: IORef (Maybe String) -> SyckParser -> CString -> IO ()
errorCallback err parser cstr = do
    msg     <- peekCString cstr
    lineptr <- #{peek SyckParser, lineptr} parser :: IO CChar
    cursor  <- #{peek SyckParser, cursor} parser  :: IO CChar
    linect  <- #{peek SyckParser, linect} parser  :: IO CChar
    writeIORef err . Just $ concat
        [ msg
        , ": line ", show (1 + fromEnum linect)
        , ", column ", show (cursor - lineptr)
        ]

freezeNode :: YamlNode -> IO (Ptr a)
freezeNode node = do
    ptr     <- newStablePtr node
    return (castPtr $ castStablePtrToPtr ptr)

thawNode :: Ptr () -> IO YamlNode
thawNode nodePtr = deRefStablePtr (castPtrToStablePtr nodePtr)

writeNode :: YamlNode -> IO SyckNodePtr
writeNode node = do
    ptr     <- newStablePtr node
    new (castPtr $ castStablePtrToPtr ptr)

readNode :: SyckParser -> SYMID -> IO YamlNode
readNode parser symId = alloca $ \nodePtr -> do
    syck_lookup_sym parser symId nodePtr
    ptr     <- peek . castPtr =<< peek nodePtr
    deRefStablePtr (castPtrToStablePtr ptr)

syckNodeTag :: SyckNode -> IO (Maybe String)
syckNodeTag syckNode = do
    tag <- #{peek SyckNode, type_id} syckNode
    if (tag == nullPtr) then (return Nothing) else do
        tag' <- peekCString tag
        return $ Just tag'

syckNodeKind :: SyckNode -> IO SyckKind
syckNodeKind syckNode = fmap toEnum $ #{peek SyckNode, kind} syckNode

syckNodeLength :: SyckKind -> SyckNode -> IO CLong
syckNodeLength SyckMap = (#{peek struct SyckMap, idx} =<<) . #{peek SyckNode, data}
syckNodeLength SyckSeq = (#{peek struct SyckSeq, idx} =<<) . #{peek SyckNode, data}
syckNodeLength SyckStr = (#{peek struct SyckStr, len} =<<) . #{peek SyckNode, data}

parseNode :: SyckKind -> SyckParser -> SyckNode -> CLong -> IO YamlNode
parseNode SyckMap parser syckNode len = do
    tag   <- syckNodeTag syckNode
    pairs <- (`mapM` [0..len-1]) $ \idx -> do
        keyId   <- syck_map_read syckNode 0 idx
        key     <- readNode parser keyId
        valId   <- syck_map_read syckNode 1 idx
        val     <- readNode parser valId
        return (key, val)
    return $ nilNode{ el = YamlMap pairs, tag = tag}

parseNode SyckSeq parser syckNode len = do
    tag   <- syckNodeTag syckNode
    nodes <- (`mapM` [0..len-1]) $ \idx -> do
        symId   <- syck_seq_read syckNode idx
        readNode parser symId
    return $ nilNode{ el = YamlSeq nodes, tag = tag }

parseNode SyckStr _ syckNode len = do
    tag   <- syckNodeTag syckNode
    cstr  <- syck_str_read syckNode
    str   <- peekCStringLen (cstr, fromEnum len)
    return $ nilNode{ el = YamlStr str, tag = tag }

foreign import ccall "wrapper"  
    mkNodeCallback :: SyckNodeHandler -> IO (FunPtr SyckNodeHandler)

foreign import ccall "wrapper"  
    mkErrorCallback :: SyckErrorHandler -> IO (FunPtr SyckErrorHandler)

foreign import ccall "wrapper"
    mkOutputCallback :: SyckOutputHandler -> IO (FunPtr SyckOutputHandler)

foreign import ccall "wrapper"
    mkEmitterCallback :: SyckEmitterHandler -> IO (FunPtr SyckEmitterHandler)

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

foreign import ccall
    syck_new_emitter :: IO SyckEmitter

foreign import ccall
    syck_free_emitter :: SyckEmitter -> IO ()

foreign import ccall
    syck_emitter_handler :: SyckEmitter -> FunPtr SyckEmitterHandler -> IO ()

foreign import ccall
    syck_output_handler :: SyckEmitter -> FunPtr SyckOutputHandler -> IO ()

foreign import ccall
    syck_emit :: SyckEmitter -> CLong -> IO ()

foreign import ccall
    syck_emitter_flush :: SyckEmitter -> CLong -> IO ()

foreign import ccall
    syck_emit_scalar :: SyckEmitter -> CString -> CInt -> CInt -> CInt -> CInt -> CString -> CInt -> IO ()

foreign import ccall
    syck_emit_seq :: SyckEmitter -> CString -> CInt -> IO ()

foreign import ccall
    syck_emit_item :: SyckEmitter -> SyckNodePtr -> IO ()

foreign import ccall
    syck_emit_end :: SyckEmitter -> IO ()

foreign import ccall
    syck_emit_map :: SyckEmitter -> CString -> CInt -> IO ()

foreign import ccall
    syck_emit_tag :: SyckEmitter -> CString -> CString -> IO ()

