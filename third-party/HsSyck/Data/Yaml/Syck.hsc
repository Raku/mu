#include "syck.h"

module Data.Yaml.Syck (
    parseYaml, emitYaml,
    parseYamlFile, emitYamlFile,
    parseYamlBytes, emitYamlBytes,
    YamlNode(..), YamlElem(..), YamlAnchor(..),
    tagNode, nilNode, mkNode, mkTagNode, mkTagStrNode, SYMID,
    packBuf, unpackBuf,
) where

import Control.Exception (bracket)
import Data.IORef
import Data.Generics
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Storable
import GHC.Ptr (Ptr(..))
import qualified Data.HashTable as Hash
import qualified Data.ByteString as Buf
import qualified Data.ByteString.Char8 as Char8

type Buf        = Buf.ByteString
type YamlTag    = Maybe Buf
data YamlAnchor
    = MkYamlAnchor    !Int
    | MkYamlReference !Int
    | MkYamlSingleton
    deriving (Show, Ord, Eq, Typeable, Data)

type SYMID = CULong

instance Data SYMID where
  toConstr x = mkIntConstr (mkIntType "Foreign.C.Types.CULong") (fromIntegral x)
  gunfold _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> error "gunfold"
  dataTypeOf _ = mkIntType "Foreign.C.Types.CULong"

data YamlNode = MkYamlNode
    { nodeId        :: !SYMID
    , nodeElem      :: !YamlElem
    , nodeTag       :: !YamlTag
    , nodeAnchor    :: !YamlAnchor
    }
    deriving (Show, Ord, Eq, Typeable, Data)

data YamlElem
    = YamlMap [(YamlNode, YamlNode)]
    | YamlSeq [YamlNode]
    | YamlStr !Buf
    | YamlNil
    deriving (Show, Ord, Eq, Typeable, Data)

type SyckNode = Ptr ()
type SyckParser = Ptr ()
type SyckNodeHandler = SyckParser -> SyckNode -> IO SYMID
type SyckErrorHandler = SyckParser -> CString -> IO ()
type SyckBadAnchorHandler = SyckParser -> CString -> IO SyckNode
type SyckNodePtr = Ptr CString
type SyckEmitter = Ptr ()  
type SyckEmitterHandler = SyckEmitter -> Ptr () -> IO ()
type SyckOutputHandler = SyckEmitter -> CString -> CLong -> IO ()
data SyckKind = SyckMap | SyckSeq | SyckStr
    deriving (Show, Ord, Eq, Enum)

nilNode :: YamlNode
nilNode = MkYamlNode 0 YamlNil Nothing MkYamlSingleton

{-# INLINE unpackBuf #-}
unpackBuf :: Buf -> String
unpackBuf = Char8.unpack

{-# INLINE packBuf #-}
packBuf :: String -> Buf
packBuf = Char8.pack

tagNode :: YamlTag -> YamlNode -> YamlNode
tagNode _ MkYamlNode{nodeTag=Just x} = error $ "can't add tag: already tagged with" ++ unpackBuf x
tagNode tag node                   = node{nodeTag = tag}

mkNode :: YamlElem -> YamlNode
mkNode x = MkYamlNode 0 x Nothing MkYamlSingleton

mkTagNode :: String -> YamlElem -> YamlNode
mkTagNode tag e = MkYamlNode 0 e (Just $! packBuf tag) MkYamlSingleton

mkTagStrNode :: String -> String -> YamlNode
mkTagStrNode tag str = mkTagNode tag (YamlStr $! packBuf str)

-- the extra commas here are not a bug
#enum CInt, , scalar_none, scalar_1quote, scalar_2quote, scalar_fold, scalar_literal, scalar_plain
#enum CInt, , seq_none, seq_inline
#enum CInt, , map_none, map_inline

{-
#def typedef void* EmitterExtras;
type EmitterExtras = Ptr ()
-}

emitYamlBytes :: YamlNode -> IO Buf
emitYamlBytes node = do
    bracket syck_new_emitter syck_free_emitter $ \emitter -> do
        -- set up output port
        out    <- newIORef []
        #{poke SyckEmitter, style} emitter scalarFold
        -- #{poke SyckEmitter, sort_keys} emitter (1 :: CInt)
        #{poke SyckEmitter, anchor_format} emitter (Ptr "%d"## :: CString)

        marks <- Hash.new (==) (Hash.hashInt)

        let freeze = freezeNode marks
        syck_emitter_handler emitter =<< mkEmitterCallback (emitterCallback freeze)
        syck_output_handler emitter =<< mkOutputCallback (outputCallbackPS out)

        markYamlNode freeze emitter node

        nodePtr <- freeze node
        let nodePtr' = fromIntegral $ nodePtr `minusPtr` nullPtr
        syck_emit emitter nodePtr'
        syck_emitter_flush emitter 0
        fmap (Buf.concat . reverse) (readIORef out)

emitYamlFile :: FilePath -> YamlNode -> IO ()
emitYamlFile file node = do
    buf <- emitYamlBytes node
    Buf.writeFile file buf

emitYaml :: YamlNode -> IO String
emitYaml node = fmap unpackBuf (emitYamlBytes node)

markYamlNode :: (YamlNode -> IO SyckNodePtr) -> SyckEmitter -> YamlNode -> IO ()
{-
markYamlNode marks emitter MkYamlNode{ anchor = MkYamlReference n } = do
    Just nodePtr <- Hash.lookup marks n
    syck_emitter_mark_node emitter nodePtr
    return ()
-}
markYamlNode freeze emitter node = do
    nodePtr <- freeze node
    rv      <- syck_emitter_mark_node emitter nodePtr
    if rv == 0 then return () else do
    case nodeElem node of
        YamlMap xs  -> sequence_ [ mark x >> mark y | (x, y) <- xs ]
        YamlSeq xs  -> mapM_ mark xs
        _           -> return ()
    where
    mark = markYamlNode freeze emitter

outputCallbackPS :: IORef [Buf] -> SyckEmitter -> CString -> CLong -> IO ()
outputCallbackPS out emitter buf len = do
    str <- Buf.copyCStringLen (buf, fromEnum len)
    str `seq` modifyIORef out (str:)

outputCallback :: SyckEmitter -> CString -> CLong -> IO ()
outputCallback emitter buf len = do
    outPtr  <- #{peek SyckEmitter, bonus} emitter
    out     <- deRefStablePtr (castPtrToStablePtr outPtr)
    str     <- peekCStringLen (buf, fromIntegral len)
    modifyIORef out (++ str)

emitterCallback :: (YamlNode -> IO SyckNodePtr) -> SyckEmitter -> Ptr () -> IO ()
emitterCallback f e vp = emitNode f e =<< thawNode vp

emitNode :: (YamlNode -> IO SyckNodePtr) -> SyckEmitter -> YamlNode -> IO ()
emitNode _ e n@(MkYamlNode{nodeElem = YamlNil}) = do
    withTag n (Ptr "string"##) $ \tag ->
        syck_emit_scalar e tag scalarNone 0 0 0 (Ptr "~"##) 1

emitNode _ e n@(MkYamlNode{nodeElem = YamlStr s}) | Buf.length s == 1, Buf.head s == 0x7E = do
    withTag n (Ptr "string"##) $ \tag ->
        syck_emit_scalar e tag scalar1quote 0 0 0 (Ptr "~"##) 1

emitNode _ e n@(MkYamlNode{nodeElem = YamlStr str}) = do
    withTag n (Ptr "string"##) $ \tag ->
        Buf.unsafeUseAsCStringLen str $ \(cs, l) ->       
        syck_emit_scalar e tag scalarNone 0 0 0 cs (toEnum l)

emitNode freeze e n@(MkYamlNode{nodeElem = YamlSeq sq}) = do
    withTag n (Ptr "array"##) $ \tag ->
        syck_emit_seq e tag seqNone
    mapM_ (syck_emit_item e) =<< mapM freeze sq
    syck_emit_end e

emitNode freeze e n@(MkYamlNode{nodeElem = YamlMap m}) = do
    withTag n (Ptr "map"##) $ \tag ->
        syck_emit_map e tag mapNone
    flip mapM_ m (\(k,v) -> do
        syck_emit_item e =<< freeze k
        syck_emit_item e =<< freeze v)
    syck_emit_end e

withTag :: YamlNode -> CString -> (CString -> IO a) -> IO a
withTag node def f = maybe (f def) (`Buf.useAsCString` f) (nodeTag node)

parseYaml :: String -> IO YamlNode
parseYaml = (`withCString` parseYamlCStr)

parseYamlFile :: String -> IO YamlNode
parseYamlFile file = parseYamlBytes =<< Buf.readFile file

parseYamlBytes :: Buf -> IO YamlNode
parseYamlBytes = (`Buf.useAsCString` parseYamlCStr)

parseYamlCStr :: CString -> IO YamlNode
parseYamlCStr cstr = do
    bracket syck_new_parser syck_free_parser $ \parser -> do
        err <- newIORef Nothing
        syck_parser_str_auto parser cstr nullFunPtr
        syck_parser_handler parser =<< mkNodeCallback nodeCallback
        syck_parser_error_handler parser =<< mkErrorCallback (errorCallback err)
        syck_parser_bad_anchor_handler parser =<< mkBadHandlerCallback badAnchorHandlerCallback
        syck_parser_implicit_typing parser 0
        syck_parser_taguri_expansion parser 0
        symId <- syck_parse parser
        if symId /= 0 then readNode parser symId else do
        rv <- readIORef err
        case rv of
            Nothing     -> return nilNode
            Just e      -> fail e

nodeCallback :: SyckParser -> SyckNode -> IO SYMID
nodeCallback parser syckNode = do
    kind    <- syckNodeKind syckNode
    len     <- syckNodeLength kind syckNode
    node    <- parseNode kind parser syckNode len
    nodePtr <- writeNode node
    symId   <- syck_add_sym parser nodePtr
    return (fromIntegral symId)

badAnchorHandlerCallback :: SyckBadAnchorHandler
badAnchorHandlerCallback _parser _cstr = do
    fail "moose!"

errorCallback :: IORef (Maybe String) -> SyckErrorHandler
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

freezeNode :: Hash.HashTable Int (Ptr a) -> YamlNode -> IO (Ptr a)
freezeNode nodes MkYamlNode{ nodeAnchor = MkYamlReference n } = do
    Just ptr <- Hash.lookup nodes n
    return ptr
freezeNode nodes node = do
    ptr     <- newStablePtr node
    let ptr' = castPtr $ castStablePtrToPtr ptr
    case nodeAnchor node of
        MkYamlAnchor n -> do
            Hash.insert nodes n ptr'
            return ptr'
        _              -> return ptr'

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

{-# NOINLINE _tagLiteral #-}
{-# NOINLINE  _colonLiteral #-}
_tagLiteral :: Buf
_tagLiteral   = packBuf "tag:"
_colonLiteral :: Buf
_colonLiteral = packBuf ":"

syckNodeTag :: SyckNode -> IO (Maybe Buf)
syckNodeTag syckNode = do
    tag <- #{peek SyckNode, type_id} syckNode
    if (tag == nullPtr) then (return Nothing) else do
        p <- Buf.copyCString tag
        return $! case Buf.elemIndex 0x2F p of -- '/'
            Just n -> let { pre = Buf.take n p; post = Buf.drop (n+1) p } in
                Just $ Buf.concat [_tagLiteral, pre, _colonLiteral, post]
            Nothing -> Nothing

syckNodeKind :: SyckNode -> IO SyckKind
syckNodeKind syckNode = do
    kind <- #{peek SyckNode, kind} syckNode
    return $ toEnum (kind `mod` 4)

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
    return $ nilNode{ nodeElem = YamlMap pairs, nodeTag = tag}

parseNode SyckSeq parser syckNode len = do
    tag   <- syckNodeTag syckNode
    nodes <- (`mapM` [0..len-1]) $ \idx -> do
        symId   <- syck_seq_read syckNode idx
        readNode parser symId
    return $ nilNode{ nodeElem = YamlSeq nodes, nodeTag = tag }

parseNode SyckStr _ syckNode len = do
    tag   <- syckNodeTag syckNode
    cstr  <- syck_str_read syckNode
    buf   <- Buf.copyCStringLen (cstr, fromEnum len)
    let node = nilNode{ nodeElem = YamlStr buf, nodeTag = tag }
    if tag == Nothing && Buf.length buf == 1 && Buf.index buf 0 == 0x7E
        then do
            style <- syck_str_style syckNode
            if style == scalarPlain
                then return nilNode
                else return node
        else return node

foreign import ccall "wrapper"  
    mkNodeCallback :: SyckNodeHandler -> IO (FunPtr SyckNodeHandler)

foreign import ccall "wrapper"  
    mkErrorCallback :: SyckErrorHandler -> IO (FunPtr SyckErrorHandler)

foreign import ccall "wrapper"  
    mkBadHandlerCallback :: SyckBadAnchorHandler -> IO (FunPtr SyckBadAnchorHandler)

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
    syck_parser_bad_anchor_handler :: SyckParser -> FunPtr SyckBadAnchorHandler -> IO ()

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
    syck_str_style :: SyckNode -> IO CInt

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
    syck_emitter_mark_node :: SyckEmitter -> SyckNodePtr -> IO SYMID

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
