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
import Data.Maybe (fromJust)
import Data.Generics
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Storable
import System.IO.Unsafe
import GHC.Ptr (Ptr(..))
import qualified Data.HashTable as Hash
import qualified Data.ByteString.Char8 as Buf
import Data.ByteString.Char8 (useAsCStringLen, useAsCString)
import Data.ByteString.Unsafe (unsafePackCString, unsafePackCStringLen)

type Buf        = Buf.ByteString
type YamlTag    = Maybe Buf
data YamlAnchor
    = AAnchor    !Int
    | AReference !Int
    | ASingleton
    deriving (Show, Ord, Eq, Typeable, Data)

type SYMID = CULong

instance Data SYMID where
  toConstr x = mkIntConstr (mkIntType "Foreign.C.Types.CULong") (fromIntegral x)
  gunfold _ z c = case constrRep c of
                    (IntConstr x) -> z (fromIntegral x)
                    _ -> error "gunfold"
  dataTypeOf _ = mkIntType "Foreign.C.Types.CULong"

data YamlNode = MkNode
    { n_id        :: !SYMID
    , n_elem      :: !YamlElem
    , n_tag       :: !YamlTag
    , n_anchor    :: !YamlAnchor
    }
    deriving (Show, Ord, Eq, Typeable, Data)

data YamlElem
    = EMap [(YamlNode, YamlNode)]
    | ESeq [YamlNode]
    | EStr !Buf
    | ENil
    deriving (Show, Ord, Eq, Typeable, Data)

type SyckNode = Ptr ()
type SyckParser = Ptr ()
type SyckNodeHandler = SyckParser -> SyckNode -> IO SYMID
type SyckErrorHandler = SyckParser -> CString -> IO ()
type SyckBadAnchorHandler = SyckParser -> CString -> IO SyckNodePtr
type SyckNodePtr = Ptr CString
type SyckEmitter = Ptr ()
type SyckEmitterHandler = SyckEmitter -> Ptr () -> IO ()
type SyckOutputHandler = SyckEmitter -> CString -> CLong -> IO ()
data SyckKind = SyckMap | SyckSeq | SyckStr
    deriving (Show, Ord, Eq, Enum)

maxId :: SYMID
maxId = maxBound

nilNode :: YamlNode
nilNode = MkNode maxId ENil Nothing ASingleton

-- | Convert a ByteString buffer into a regular Haskell string

{-# INLINE unpackBuf #-}
unpackBuf :: Buf -> String
unpackBuf = Buf.unpack

-- | Convert a regular Haskell string into a ByteString buffer

{-# INLINE packBuf #-}
packBuf :: String -> Buf
packBuf = Buf.pack

tagNode :: YamlTag -> YamlNode -> YamlNode
tagNode _ MkNode{n_tag=Just x} = error $ "can't add tag: already tagged with" ++ unpackBuf x
tagNode tag node               = node{n_tag = tag}

mkNode :: YamlElem -> YamlNode
mkNode x = MkNode maxId x Nothing ASingleton

mkTagNode :: String -> YamlElem -> YamlNode
mkTagNode tag e = MkNode maxId e (Just $! packBuf tag) ASingleton

mkTagStrNode :: String -> String -> YamlNode
mkTagStrNode tag str = mkTagNode tag (EStr $! packBuf str)

-- the extra commas here are not a bug
#enum CInt, , scalar_none, scalar_1quote, scalar_2quote, scalar_fold, scalar_literal, scalar_plain
#enum CInt, , seq_none, seq_inline
#enum CInt, , map_none, map_inline

{-
#def typedef void* EmitterExtras;
type EmitterExtras = Ptr ()
-}

-- | Dump a YAML node into a ByteString buffer (fast)

emitYamlBytes :: YamlNode -> IO Buf
emitYamlBytes node = do
    bracket syck_new_emitter syck_free_emitter $ \emitter -> do
        -- set up output port
        out    <- newIORef []
        #{poke SyckEmitter, style} emitter scalarFold
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

-- | Given a file name, dump a YAML node into that file

emitYamlFile :: FilePath -> YamlNode -> IO ()
emitYamlFile file node = do
    buf <- emitYamlBytes node
    Buf.writeFile file buf

-- | Dump a YAML node into a regular Haskell string

emitYaml :: YamlNode -> IO String
emitYaml node = fmap unpackBuf (emitYamlBytes node)

markYamlNode :: (YamlNode -> IO SyckNodePtr) -> SyckEmitter -> YamlNode -> IO ()
{-
markYamlNode marks emitter MkNode{ anchor = AReference n } = do
    Just nodePtr <- Hash.lookup marks n
    syck_emitter_mark_node emitter nodePtr
    return ()
-}
markYamlNode freeze emitter node = do
    nodePtr <- freeze node
    rv      <- syck_emitter_mark_node emitter nodePtr
    if rv == 0 then return () else do
    case n_elem node of
        EMap xs  -> sequence_ [ mark x >> mark y | (x, y) <- xs ]
        ESeq xs  -> mapM_ mark xs
        _        -> return ()
    where
    mark = markYamlNode freeze emitter

outputCallbackPS :: IORef [Buf] -> SyckEmitter -> CString -> CLong -> IO ()
outputCallbackPS out emitter buf len = do
    str <- unsafePackCStringLen (buf, fromEnum len)
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
emitNode _ e n | n_elem n == ENil = do
    withTag n (Ptr "string"##) $ \tag ->
        syck_emit_scalar e tag scalarNone 0 0 0 (Ptr "~"##) 1

emitNode _ e n | EStr s <- n_elem n, Buf.length s == 1, Buf.head s == '~' = do
    withTag n (Ptr "string"##) $ \tag ->
        syck_emit_scalar e tag scalar1quote 0 0 0 (Ptr "~"##) 1

emitNode _ e n | EStr s <- n_elem n = do
    withTag n (Ptr "string"##) $ \tag ->
        useAsCStringLen s $ \(cs, l) ->
        syck_emit_scalar e tag scalarFold 0 0 0 cs (toEnum l)

emitNode freeze e n | ESeq sq <- n_elem n = do
    withTag n (Ptr "array"##) $ \tag ->
--        syck_emit_seq e tag seqInline
      syck_emit_seq e tag seqNone
    mapM_ (syck_emit_item e) =<< mapM freeze sq
    syck_emit_end e

emitNode freeze e n | EMap m <- n_elem n = do
    withTag n (Ptr "map"##) $ \tag ->
--        syck_emit_map e tag mapInline
      syck_emit_map e tag mapNone
    flip mapM_ m (\(k,v) -> do
        syck_emit_item e =<< freeze k
        syck_emit_item e =<< freeze v)
    syck_emit_end e

withTag :: YamlNode -> CString -> (CString -> IO a) -> IO a
withTag node def f = maybe (f def) (`useAsCString` f) (n_tag node)

-- | Parse a regular Haskell string

parseYaml :: String -> IO YamlNode
parseYaml = (`withCString` parseYamlCStr)

-- | Given a file name, parse contents of file

parseYamlFile :: String -> IO YamlNode
parseYamlFile file = parseYamlBytes =<< Buf.readFile file

-- | Parse a ByteString buffer (this is faster)

parseYamlBytes :: Buf -> IO YamlNode
parseYamlBytes = (`useAsCString` parseYamlCStr)

parseYamlCStr :: CString -> IO YamlNode
parseYamlCStr cstr = do
    bracket syck_new_parser syck_free_parser $ \parser -> do
        err     <- newIORef Nothing
        badancs <- Hash.new (==) Hash.hashInt
        syck_parser_str_auto parser cstr nullFunPtr
        syck_parser_handler parser =<< mkNodeCallback (nodeCallback badancs)
        syck_parser_error_handler parser =<< mkErrorCallback (errorCallback err)
        syck_parser_bad_anchor_handler parser =<< mkBadHandlerCallback (badAnchorHandlerCallback badancs)
        syck_parser_implicit_typing parser 0
        syck_parser_taguri_expansion parser 0
        symId <- syck_parse parser
        if symId /= 0 then readNode parser symId else do
        rv <- readIORef err
        case rv of
            Nothing     -> return nilNode
            Just e      -> fail e

type BadAnchorTable = Hash.HashTable Int YamlNode


nodeCallback :: BadAnchorTable -> SyckParser -> SyckNode -> IO SYMID
nodeCallback badancs parser syckNode = mdo
    nodeId  <- #{peek SyckNode, id} syckNode
    kind    <- syckNodeKind syckNode

    let makeRegularNode = do
        len     <- syckNodeLength kind syckNode
        parseNode kind parser syckNode len symId

    node    <- case kind of
        SyckMap -> do
            rv  <- Hash.lookup badancs (syckNode `minusPtr` nullPtr)
            case rv of
                Just{}  -> do
                    -- print ("bad anchor wanted", syckNode)
                    unsafeInterleaveIO (fmap fromJust (Hash.lookup badancs (nodePtr `minusPtr` nullPtr)))
                _       -> makeRegularNode
        _       -> makeRegularNode
    nodePtr <- writeNode node

    -- Do something here about circular refs.
    case nodeId :: SYMID of
        0   -> return False
        _   -> alloca $ \origPtr -> do
            syck_lookup_sym parser nodeId origPtr
            ptr <- peek origPtr
            -- print ("bad anchor handled", nodeId, ptr)
            Hash.update badancs (ptr `minusPtr` nullPtr) node

    symId   <- fmap fromIntegral (syck_add_sym parser nodePtr)

    return symId

badAnchorHandlerCallback :: BadAnchorTable -> SyckBadAnchorHandler
badAnchorHandlerCallback badancs parser cstr = do
    syckNode    <- syck_alloc_map
    -- msg         <- peekCString cstr
    -- print ("bad anchor encountered", syckNode)
    Hash.insert badancs (syckNode `minusPtr` nullPtr) (error "unhandled bad anchor")
    return syckNode

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
freezeNode nodes MkNode{ n_anchor = AReference n } = do
    rv <- Hash.lookup nodes n
    case rv of
        Just ptr    -> return ptr
        _           -> fail $ "Failed to resolve reference: " ++ show n
freezeNode nodes node = do
    ptr     <- newStablePtr node
    case n_anchor node of
        AAnchor n -> do
            rv <- Hash.lookup nodes n
            case rv of
                Just ptr -> return ptr
                _ -> do
                    ptr     <- newStablePtr node
                    let ptr' = castPtr (castStablePtrToPtr ptr)
                    Hash.insert nodes n ptr'
                    return $! ptr'
        _         -> do
            ptr     <- newStablePtr node
            return $! castPtr (castStablePtrToPtr ptr)

thawNode :: Ptr () -> IO YamlNode
thawNode nodePtr = deRefStablePtr (castPtrToStablePtr nodePtr)

writeNode :: YamlNode -> IO SyckNodePtr
writeNode node = do
    ptr <- newStablePtr node
    new (castPtr $ castStablePtrToPtr ptr)

readNode :: SyckParser -> SYMID -> IO YamlNode
readNode parser symId = alloca $ \nodePtr -> do
    syck_lookup_sym parser symId nodePtr
    ptr <- peek . castPtr =<< peek nodePtr
    deRefStablePtr (castPtrToStablePtr ptr)

{-# NOINLINE _tagLiteral #-}
{-# NOINLINE _colonLiteral #-}
_tagLiteral, _colonLiteral :: Buf
_tagLiteral   = packBuf "tag:"
_colonLiteral = packBuf ":"

syckNodeTag :: SyckNode -> IO (Maybe Buf)
syckNodeTag syckNode = do
    tag <- #{peek SyckNode, type_id} syckNode
    if (tag == nullPtr) then (return Nothing) else do
        p <- unsafePackCString tag
        return $! case Buf.elemIndex '/' p of
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

parseNode :: SyckKind -> SyckParser -> SyckNode -> CLong -> SYMID -> IO YamlNode
parseNode SyckMap parser syckNode len nid = do
    tag   <- syckNodeTag syckNode
    pairs <- (`mapM` [0..len-1]) $ \idx -> do
        keyId   <- syck_map_read syckNode 0 idx
        key     <- readNode parser keyId
        valId   <- syck_map_read syckNode 1 idx
        val     <- readNode parser valId
        return (key, val)
    return $ nilNode{ n_elem = EMap pairs, n_tag = tag, n_id = nid }

parseNode SyckSeq parser syckNode len nid = do
    tag   <- syckNodeTag syckNode
    nodes <- (`mapM` [0..len-1]) $ \idx -> do
        symId   <- syck_seq_read syckNode idx
        readNode parser symId
    return $ nilNode{ n_elem = ESeq nodes, n_tag = tag, n_id = nid }

parseNode SyckStr _ syckNode len nid = do
    tag   <- syckNodeTag syckNode
    cstr  <- syck_str_read syckNode
    buf   <- unsafePackCStringLen (cstr, fromEnum len)
    let node = nilNode{ n_elem = EStr buf, n_tag = tag, n_id = nid }
    if tag == Nothing && Buf.length buf == 1 && Buf.index buf 0 == '~'
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
    syck_alloc_map :: IO SyckNodePtr

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

-- vim: syntax=haskell :
