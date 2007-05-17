{-# OPTIONS_GHC -fglasgow-exts -funbox-strict-fields -fallow-overlapping-instances -fvia-C -fparr #-}

module DrIFT.YAML where
import Data.Yaml.Syck
import Data.Ratio
import GHC.Exts
import Data.Typeable
import Data.Char
import Control.Exception
import Control.Concurrent.STM
import Foreign.Ptr
import Control.Monad.Reader
import GHC.PArr
import System.IO.Unsafe
import Data.IORef
import Data.Bits
import Data.List	( foldl' )
import Data.Int		( Int32, Int64 )
import Pugs.Internals (encodeUTF8, decodeUTF8, addressOf)
import Data.HashTable (HashTable)
import qualified UTF8 as Buf
import qualified Data.ByteString as Bytes
import qualified Data.IntSet as IntSet
import qualified Data.HashTable as Hash

type Buf = Buf.ByteString

type YAMLClass = String
type YAMLKey = String
type YAMLVal = YamlNode
type SeenCache = IORef IntSet.IntSet

toYamlNode :: YAML a => a -> IO YamlNode
toYamlNode x = do
    cache   <- newIORef IntSet.empty 
    runReaderT (asYAML x) cache

showYaml :: YAML a => a -> IO String
showYaml x = do
    node <- toYamlNode x
    emitYaml node

showYamlCompressed :: YAML a => a -> IO String
showYamlCompressed = showYaml
{-
showYamlCompressed x = do
    node    <- toYamlNode x
    node'   <- compressYamlNode node
    emitYaml node'
-}

type EmitAs = ReaderT SeenCache IO

class Typeable a => YAML a where
    asYAML :: a -> EmitAs YamlNode
    asYAML x = lift $ do
        ty <- Control.Exception.handle (const $ return "()") $
            evaluate (show (typeOf x))
        case ty of
            "()" -> return nilNode
            _    -> return $ mkTagNode (tagHs ty) ENil
    fromYAML :: YamlNode -> IO a
    fromYAML = fromYAMLElem . n_elem
    fromYAMLElem :: YamlElem -> IO a
    fromYAMLElem e = do
        fail $ "unhandled element: " ++ (show e) ++ ", expecting " ++ show (typeOf (undefined :: a))

asYAMLseq :: YAMLClass -> [EmitAs YAMLVal] -> EmitAs YamlNode
asYAMLseq c ps = do
    ps' <- sequence ps
    return $ mkTagNode (tagHs c) (ESeq ps')

asYAMLmap :: YAMLClass -> [(YAMLKey, EmitAs YAMLVal)] -> EmitAs YamlNode
asYAMLmap c ps = do
    ps' <- mapM asYAMLpair ps
    return $ mkTagNode (tagHs c) (EMap ps')
    where
    asYAMLpair (k, v) = do
        k' <- asYAML k
        v' <- v
        return (k', v')

asYAMLmapBuf :: YAMLClass -> [(Buf.ByteString, EmitAs YAMLVal)] -> EmitAs YamlNode
asYAMLmapBuf c ps = do
    ps' <- mapM asYAMLpair ps
    return $ mkTagNode (tagHs c) (EMap ps')
    where
    asYAMLpair (k, v) = do
        k' <- asYAML k
        v' <- v
        return (k', v')

fromYAMLseq :: YAML a => YamlNode -> IO [a]
fromYAMLseq MkNode{n_elem=ESeq m} = mapM fromYAML m
fromYAMLseq e = fail $ "no parse: " ++ show e

fromYAMLmap :: YAML a => YamlNode -> IO [(String, a)]
fromYAMLmap MkNode{n_elem=EMap m} = mapM fromYAMLpair m
    where
    fromYAMLpair (MkNode{n_elem=EStr k}, v) = do
        v' <- fromYAML v
        return (unpackBuf k, v')
    fromYAMLpair e = fail $ "no parse: " ++ show e
fromYAMLmap e = fail $ "no parse: " ++ show e

fromYAMLmapBuf :: YAML a => YamlNode -> IO [(Buf.ByteString, a)]
fromYAMLmapBuf MkNode{n_elem=EMap m} = mapM fromYAMLpair m
    where
    fromYAMLpair (MkNode{n_elem=EStr k}, v) = do
        v' <- fromYAML v
        return (k, v')
    fromYAMLpair e = fail $ "no parse: " ++ show e
fromYAMLmapBuf e = fail $ "no parse: " ++ show e

asYAMLcls :: YAMLClass -> EmitAs YamlNode
asYAMLcls c = return $ mkTagStrNode (tagHs c) c

tagHs :: YAMLClass -> String
tagHs = ("tag:hs:" ++)

deTag :: YamlNode -> YAMLClass
deTag MkNode{n_tag=Just s} =
    case s' of
        't':'a':'g':':':'h':'s':':':tag -> tag
        tag                             -> error $ "not a Haskell tag: " ++ tag
    where s' = unpackBuf s
deTag n = error $ "missing tag: " ++ show n

instance YAML () where
    asYAML _ = return nilNode
    fromYAMLElem _ = return ()

instance YAML Int where
    asYAML x = return $ mkTagStrNode "int" $ show x
    fromYAMLElem (EStr x) = return $ read $ Buf.unpack x
    fromYAMLElem e = failWith e

instance YAML Word where
    asYAML x = return $ mkTagStrNode "int" $ show x
    fromYAMLElem (EStr x) = return $ read $ Buf.unpack x
    fromYAMLElem e = failWith e

instance YAML Buf where
    asYAML = return . mkNode . EStr
    fromYAMLElem (EStr str) = return str
    fromYAMLElem e = failWith e

instance YAML String where
    asYAML = return . mkTagNode "str" . EStr . Buf.pack . encodeUTF8
    fromYAMLElem (EStr str) = return . decodeUTF8 $ Buf.unpack str
    fromYAMLElem e = failWith e

instance YAML Bool where
    asYAML True = return $ mkTagStrNode "bool#yes" "1"
    asYAML False = return $ mkTagStrNode "bool#no" "0"
    fromYAML MkNode{n_tag=Just s} | s == packBuf "bool#yes" = return True
    fromYAML MkNode{n_tag=Just s} | s == packBuf "bool#no"  = return False
    fromYAML MkNode{n_elem=x} = fromYAMLElem x
    fromYAMLElem (EStr x) = return (x /= packBuf "0")
    fromYAMLElem e = failWith e

instance YAML Integer where 
    asYAML x = return $ mkTagStrNode "int" $ show x
    fromYAMLElem (EStr x) = return $ read $ Buf.unpack x
    fromYAMLElem e = failWith e

instance YAML Rational where 
    asYAML r = asYAML (x, y)
        where
        x = numerator r
        y = denominator r
    fromYAMLElem (ESeq [MkNode{n_elem=EStr x}, MkNode{n_elem=EStr y}]) =
        return $ (read $ Buf.unpack x) % (read $ Buf.unpack y)
    fromYAMLElem e = failWith e
    
instance YAML Double where 
    asYAML num
        | show num == "Infinity"  = return $ mkTagStrNode "float#inf"    ".Inf"
        | show num == "-Infinity" = return $ mkTagStrNode "float#neginf" "-.Inf"
        | show num == "NaN"       = return $ mkTagStrNode "float#nan"    "-.NaN"
        | otherwise               = return $ mkTagStrNode "float"        $ show num
    fromYAML MkNode{n_tag=Just s} | s == packBuf "float#inf"    = return $  1/0 -- "Infinity" 
    fromYAML MkNode{n_tag=Just s} | s == packBuf "float#neginf" = return $ -1/0 -- "-Infinity" 
    fromYAML MkNode{n_tag=Just s} | s == packBuf "float#nan"    = return $  0/0 -- "NaN" 
    fromYAML MkNode{n_elem=x} = fromYAMLElem x
    fromYAMLElem (EStr x) = return $ read $ Buf.unpack x
    fromYAMLElem e = failWith e

instance (YAML a) => YAML (Maybe a) where
    asYAML (Just x) = asYAML x
    asYAML Nothing = return $ nilNode
    fromYAML MkNode{n_elem=ENil} = return Nothing
    fromYAML x = return . Just =<< fromYAML x
    fromYAMLElem ENil = return Nothing
    fromYAMLElem x = return . Just =<< fromYAMLElem x

instance (YAML a) => YAML [a] where
    asYAML xs = asYAMLanchor xs $ do
        xs' <- mapM asYAML xs
        (return . mkNode . ESeq) xs'
    fromYAMLElem (ESeq s) = mapM fromYAML s
    fromYAMLElem e = fail $ "no parse: " ++ show e

instance (YAML a) => YAML [:a:] where
    asYAML xs = asYAMLanchor xs $ do
        xs' <- mapM asYAML (fromP xs)
        (return . mkNode . ESeq) xs'
    fromYAMLElem (ESeq s) = fmap toP (mapM fromYAML s)
    fromYAMLElem e = fail $ "no parse: " ++ show e

instance (YAML a, YAML b) => YAML (a, b) where
    asYAML (x, y) = do
        x' <- asYAML x
        y' <- asYAML y
        return $ mkNode (ESeq [x', y'])
    fromYAMLElem (ESeq [x, y]) = do
        x' <- fromYAML x
        y' <- fromYAML y
        return (x', y')
    fromYAMLElem e = fail $ "no parse: " ++ show e

instance (YAML a, YAML b, YAML c) => YAML (a, b, c) where
    asYAML (x, y, z) = do
        x' <- asYAML x
        y' <- asYAML y
        z' <- asYAML z
        return $ mkNode (ESeq [x', y', z'])
    fromYAMLElem (ESeq [x, y, z]) = do
        x' <- fromYAML x
        y' <- fromYAML y
        z' <- fromYAML z
        return (x', y', z')
    fromYAMLElem e = fail $ "no parse: " ++ show e

{-# NOINLINE seen #-}
seen :: Hash.HashTable SYMID Any
seen = unsafePerformIO (Hash.new (==) fromIntegral)

instance (Typeable a, YAML a) => YAML (TVar a) where
    asYAML = asYAMLwith (lift . atomically . readTVar)
    fromYAML x = do
        -- If this node is seen, then don't bother -- just read from it.
        let nid = n_id x
        rv  <- Hash.lookup seen nid
        case rv of
            Just x  -> return (unsafeCoerce# x)
            _       -> do
                tv  <- newTVarIO (error "moose")
                Hash.insert seen nid (unsafeCoerce# tv)
                j   <- fromYAML x
                atomically (writeTVar tv j)
                return tv
    fromYAMLElem = (newTVarIO =<<) . fromYAMLElem


asYAMLanchor :: a -> EmitAs YamlNode -> EmitAs YamlNode
asYAMLanchor x m = do
    cache   <- ask
    seen    <- liftIO $ readIORef cache
    let ptr = fromEnum (addressOf x)
    if IntSet.member ptr seen
        then return nilNode{ n_anchor = AReference ptr } 
        else do
            liftIO $ modifyIORef cache (IntSet.insert ptr)
            rv  <- m
            return rv{ n_anchor = AAnchor ptr }

asYAMLwith :: (YAML a, YAML b) => (a -> EmitAs b) -> a -> EmitAs YamlNode
asYAMLwith f x = asYAMLanchor x (asYAML =<< f x)

failWith :: forall a. YAML a => YamlElem -> IO a
failWith e = fail $ "no parse: " ++ show e ++ " as " ++ show typ
    where
    typ :: TypeRep
    typ = typeOf (undefined :: a)


type SeenHash = HashTable SYMID (Maybe YamlNode)
type DuplHash = HashTable YamlNode Int

-- Compress a YAML tree by finding common subexpressions.
compressYamlNode :: YamlNode -> IO YamlNode
compressYamlNode node = do
    -- Phase 1: Update YamlNode to fill in SYMID based on hashing its values.
    --          First time a SYMID is seen, firstTime (Hash from SYMID to (Maybe YamlNode))
    --          is inserted.  Next time, both YamlNodes are written to the dupNode
    --          hash (Hash from YamlNode to Int), and firstTime now contains Nothing.
    seen    <- Hash.new (==) fromIntegral
    dupl    <- Hash.new eqNode (fromIntegral . n_id)
    count   <- newIORef 1

    let ?seenHash = seen
        ?duplHash = dupl
        ?countRef = count
    
    node' <- markNode node

    -- Phase 2: Revisit YamlNode and lookup dupNode; if it's 0 then increment curId
    --          and mark this as AAnchor; otherwise retrieve and mark this as AReference.
    visitNode node'

eqNode :: YamlNode -> YamlNode -> Bool
eqNode x@MkNode{ n_anchor = ASingleton } y@MkNode{ n_anchor = ASingleton } = (n_tag x == n_tag y) && eqElem (n_elem x) (n_elem y)
eqNode _ _ = False

eqElem :: YamlElem -> YamlElem -> Bool
eqElem ENil         ENil        = True
eqElem (EStr x)     (EStr y)    = x == y
eqElem (ESeq xs)    (ESeq ys)   = and ((length xs == length ys):zipWith eqNode xs ys)
eqElem (EMap xs)    (EMap ys)   = and ((length xs == length ys):zipWith eqPair xs ys)
    where
    eqPair (kx, vx) (ky, vy)    = eqNode kx ky && eqNode vx vy
eqElem _            _           = False


visitNode :: (?countRef :: IORef Int, ?duplHash :: DuplHash) => YamlNode -> IO YamlNode
visitNode node = do
    rv  <- Hash.lookup ?duplHash node
    case rv of
        Just 0  -> do
            i   <- readIORef ?countRef
            Hash.update ?duplHash node i 
            writeIORef ?countRef (i+1)
            elem'   <- visitElem (n_elem node)
            return node{ n_anchor = AAnchor i, n_elem = elem' }
        Just i  -> return nilNode{ n_anchor = AReference i }
        _       -> do
            elem'   <- visitElem (n_elem node)
            return node{ n_elem = elem' }

visitElem :: (?countRef :: IORef Int, ?duplHash :: DuplHash) => YamlElem -> IO YamlElem
visitElem (ESeq ns)      = fmap ESeq (mapM visitNode ns)
visitElem (EMap ps)      = fmap EMap (mapM visitPair ps)
    where
    visitPair (k, v) = do
        k'  <- visitNode k
        v'  <- visitNode v
        return (k', v')
visitElem e             = return e

markNode :: (?seenHash :: SeenHash, ?duplHash :: DuplHash) => YamlNode -> IO YamlNode
-- markNode node@MkNode{ n_anchor = AReference r } = return node
-- markNode node@MkNode{ n_anchor = AAnchor r } = return node
markNode node = do
    (symid32, elem')    <- markElem (n_elem node)
    let node' = node{ n_id = symid }
        symid = fromIntegral (iterI32s tagid symid32)
        tagid = maybe 0 Buf.hash (n_tag node)
    rv  <- Hash.lookup ?seenHash symid
    case rv of
        Just (Just prevNode)   -> do
            Hash.update ?duplHash node' 0
            Hash.update ?duplHash prevNode 0
            Hash.update ?seenHash symid Nothing
        Just _  -> Hash.update ?duplHash node' 0
        _       -> Hash.update ?seenHash symid (Just node')
    return node'{ n_elem = elem' }

markElem :: (?seenHash :: SeenHash, ?duplHash :: DuplHash) => YamlElem -> IO (Int32, YamlElem)
markElem ENil           = return (0, ENil)
markElem n@(EStr buf)   = return (Buf.hash buf, n)
markElem (ESeq ns)      = do
    ns' <- mapM markNode ns
    return (hashIDs (map n_id ns'), ESeq ns')
markElem (EMap ps)      = do
    (symid, ps') <- foldM markPair (0, []) ps
    return (symid, EMap ps')
    where
    markPair (symid, ps) (k, v) = do
        k'  <- markNode k
        v'  <- markNode v
        return (iterIDs (iterIDs symid (n_id k')) (n_id v'), ((k', v'):ps))

hashIDs :: [SYMID] -> Int32
hashIDs = foldl' iterIDs 0

iterIDs :: Int32 -> SYMID -> Int32
iterIDs m c = fromIntegral (c + 1) * golden + mulHi m golden

iterI32s :: Int32 -> Int32 -> Int32
iterI32s m c = (c + 1) * golden + mulHi m golden

golden :: Int32
golden = -1640531527

mulHi :: Int32 -> Int32 -> Int32
mulHi a b = fromIntegral (r `shiftR` 32)
    where
    r :: Int64
    r = fromIntegral a * fromIntegral b :: Int64
