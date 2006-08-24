{-# OPTIONS_GHC -fglasgow-exts -funbox-strict-fields -fallow-overlapping-instances -fvia-C #-}

module DrIFT.YAML where
import Data.Yaml.Syck
import Data.Ratio
import GHC.Exts
import Data.Typeable
import Data.Char
import Control.Exception
import Control.Concurrent.STM
import qualified Data.IntSet as IntSet
import Foreign.StablePtr
import Foreign.Ptr
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as Buf
import qualified Data.ByteString as Bytes
import Pugs.Internals (encodeUTF8, decodeUTF8)

type Buf = Buf.ByteString

type YAMLClass = String
type YAMLKey = String
type YAMLVal = YamlNode
type SeenCache = IntSet.IntSet

toYamlNode :: YAML a => a -> IO YamlNode
toYamlNode x = runReaderT (asYAML x) IntSet.empty 

showYaml :: YAML a => a -> IO String
showYaml x = do
    node <- toYamlNode x
    emitYaml node

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
    fromYAMLElem e = fail $ "unhandled element: " ++ (show e)

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

fromYAMLmap :: YAML a => YamlNode -> IO [(String, a)]
fromYAMLmap MkNode{n_elem=EMap m} = mapM fromYAMLpair m
    where
    fromYAMLpair (MkNode{n_elem=EStr k}, v) = do
        v' <- fromYAML v
        return (unpackBuf k, v')
    fromYAMLpair e = fail $ "no parse: " ++ show e
fromYAMLmap e = fail $ "no parse: " ++ show e

fromYAMLmapBuf :: YAML a => YamlNode -> IO [(Buf.ByteString, a)]
fromYAMLmapBuf MkNode{n_elem=EMap m} = doMap m
    where
    doMap [] = return []
    doMap ((MkNode{n_elem=EStr k}, v):xs) = do
        v'  <- fromYAML v
        xs' <- doMap xs
        return (v' `seq` xs' `seq` ((k, v'):xs'))
    doMap e = fail $ "no parse: " ++ show e
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
    asYAML = return .  mkTagNode "str" . EStr . Buf.pack . encodeUTF8
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
    asYAML xs = do
        xs' <- mapM asYAML xs
        (return . mkNode . ESeq) xs'
    fromYAMLElem (ESeq s) = mapM fromYAML s
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

instance (Typeable a, YAML a) => YAML (TVar a) where
    asYAML = asYAMLwith (lift . atomically . readTVar)
    fromYAML = (atomically . newTVar =<<) . fromYAML
    fromYAMLElem = (atomically . newTVar =<<) . fromYAMLElem

asYAMLwith :: (YAML a, YAML b) => (a -> EmitAs b) -> a -> EmitAs YamlNode
asYAMLwith f x = do
    ptr  <- liftIO $ addressOf x
    seen <- ask
    if IntSet.member ptr seen
        then return nilNode{ n_anchor = AReference ptr } 
        else do
            rv   <- local (IntSet.insert ptr) (asYAML =<< f x)
            return rv{ n_anchor = AAnchor ptr }

addressOf :: a -> IO Int
addressOf x = do
    ptr <- newStablePtr x
    return (castStablePtrToPtr ptr `minusPtr` (nullPtr :: Ptr ()))

failWith :: forall a. YAML a => YamlElem -> IO a
failWith e = fail $ "no parse: " ++ show e ++ " as " ++ show typ
    where
    typ :: TypeRep
    typ = typeOf (undefined :: a)

