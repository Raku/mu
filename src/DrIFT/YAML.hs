{-# OPTIONS_GHC -fglasgow-exts -funbox-strict-fields -fallow-overlapping-instances -fvia-C #-}

module DrIFT.YAML where
import Data.Yaml.Syck
import Data.Ratio
import Data.Char (chr)
import GHC.Exts
import UTF8
import Data.Typeable
import Control.Exception
import Control.Monad
import Control.Concurrent.STM
import Data.IORef
import qualified Data.IntMap as IntMap
import Foreign.StablePtr
import Foreign.Ptr
import System.IO.Unsafe
import Control.Monad.Reader
import qualified Data.FastPackedString as Str

type Str = Str.FastString

type YAMLClass = String
type YAMLKey = String
type YAMLVal = YamlNode
type SeenCache = IntMap.IntMap YamlNode

showYaml :: YAML a => a -> IO String
showYaml x = do
    node    <- (`runReaderT` IntMap.empty) (asYAML x)
    rv      <- emitYaml node
    case rv of
        Left e  -> error e
        Right s -> return s

type EmitAs = ReaderT SeenCache IO

class Typeable a => YAML a where
    asYAML :: a -> EmitAs YamlNode
    asYAML x = lift $ do
        ty <- Control.Exception.handle (const $ return "()") $
            evaluate (reverse (takeWhile (/= '.') (reverse (show (typeOf x)))))
        return $ case ty of
            "()" -> nilNode
            _    -> mkTagNode (tagHs ty) YamlNil
    fromYAML :: YamlNode -> IO a
    fromYAML MkYamlNode{el=x} = fromYAMLElem x
    fromYAMLElem :: YamlElem -> IO a

asYAMLseq :: YAMLClass -> [EmitAs YAMLVal] -> EmitAs YamlNode
asYAMLseq c ps = do
    ps' <- sequence ps
    return $ mkTagNode (tagHs c) (YamlSeq ps')

asYAMLmap :: YAMLClass -> [(YAMLKey, EmitAs YAMLVal)] -> EmitAs YamlNode
asYAMLmap c ps = do
    ps' <- mapM asYAMLpair ps
    return $ mkTagNode (tagHs c) (YamlMap ps')
    where
    asYAMLpair (k, v) = do
        k' <- asYAML k
        v' <- v
        return (k', v')

fromYAMLmap :: YAML a => YamlNode -> IO [(String, a)]
fromYAMLmap MkYamlNode{el=YamlMap m} = do
    mapM fromYAMLpair m
    where
    fromYAMLpair ~(MkYamlNode{el=YamlStr k}, v) = do
        v' <- fromYAML v
        return (Str.unpack k, v')
    

asYAMLcls :: YAMLClass -> EmitAs YamlNode
asYAMLcls c = return $ mkTagNode (tagHs c) (YamlStr $ Str.pack c)

tagHs :: YAMLClass -> String
tagHs = ("tag:hs:" ++)

deTag :: YamlNode -> YAMLClass
deTag MkYamlNode{tag=Just s} =
    let 't':'a':'g':':':'h':'s':':':tag = s' in tag
    where s' = Str.unpack s
deTag _ = error "not a Haskell tag"

instance YAML () where
    asYAML _ = return nilNode
    fromYAML _ = return ()

instance YAML Int where
    asYAML x = return $ mkTagNode "int" (YamlStr $ Str.pack $ show x)
    fromYAMLElem ~(YamlStr x) = return $ read $ Str.unpack x

instance YAML String where
    asYAML str = return $ mkTagNode "str" (YamlStr $ Str.pack str)
    fromYAMLElem ~(YamlStr str) = return $ read $ Str.unpack str

instance YAML Bool where
    asYAML True = return $ mkTagNode "bool#yes" (YamlStr $ Str.pack "1")
    asYAML False = return $ mkTagNode "bool#no" (YamlStr $ Str.pack "0")
    fromYAML MkYamlNode{tag=Just s} | s == Str.pack "bool#yes" = return True
    fromYAML MkYamlNode{tag=Just s} | s == Str.pack "bool#no"  = return False

instance YAML Integer where 
    asYAML x = return $ mkTagNode "int" (YamlStr $ Str.pack $ show x)
    fromYAMLElem ~(YamlStr x) = return $ read $ Str.unpack x

instance YAML Rational where 
    asYAML r = asYAML (x, y)
        where
        x = numerator r
        y = denominator r
    fromYAMLElem ~(YamlStr str) = return $ (read x) / (read y)
        where
        (x,y) = break (== '/') (Str.unpack str)
    
instance YAML Double where 
    asYAML num | show num == "Infinity"  = return $ mkTagNode "float#inf"    (YamlStr $ Str.pack ".Inf")
               | show num == "-Infinity" = return $ mkTagNode "float#neginf" (YamlStr $ Str.pack "-.Inf")
               | show num == "NaN"       = return $ mkTagNode "float#nan"    (YamlStr $ Str.pack "-.NaN")
               | otherwise               = return $ mkTagNode "float"        (YamlStr $ Str.pack $ show num)
    fromYAML MkYamlNode{tag=Just s} | s == Str.pack "float#inf"    = return $  1/0 -- "Infinity" 
    fromYAML MkYamlNode{tag=Just s} | s == Str.pack "float#neginf" = return $ -1/0 -- "-Infinity" 
    fromYAML MkYamlNode{tag=Just s} | s == Str.pack "float#nan"    = return $  0/0 -- "NaN" 
    fromYAML ~MkYamlNode{el=YamlStr x}                             = return $ read $ Str.unpack x

instance (YAML a) => YAML (Maybe a) where
    asYAML (Just x) = asYAML x
    asYAML Nothing = return $ nilNode
    fromYAML MkYamlNode{el=YamlNil} = return Nothing
    fromYAML x = return . Just =<< fromYAML x

instance (YAML a) => YAML [a] where
    asYAML xs = do
        xs' <- mapM asYAML xs
        (return . mkNode . YamlSeq) xs'
    fromYAML ~MkYamlNode{el=YamlSeq s} = mapM fromYAML s

instance (YAML a, YAML b) => YAML (a, b) where
    asYAML (x, y) = do
        x' <- asYAML x
        y' <- asYAML y
        return $ mkNode (YamlSeq [x', y'])
    fromYAML ~MkYamlNode{el=YamlSeq [x, y]} = do
        x' <- fromYAML x
        y' <- fromYAML y
        return (x', y')

instance (YAML a, YAML b, YAML c) => YAML (a, b, c) where
    asYAML (x, y, z) = do
        x' <- asYAML x
        y' <- asYAML y
        z' <- asYAML z
        return $ mkNode (YamlSeq [x', y', z'])
    fromYAML ~MkYamlNode{el=YamlSeq [x, y, z]} = do
        x' <- fromYAML x
        y' <- fromYAML y
        z' <- fromYAML z
        return (x', y', z')

instance (Typeable a, YAML a) => YAML (TVar a) where
    asYAML = asYAMLwith (lift . atomically . readTVar)

asYAMLwith :: (YAML a, YAML b) => (a -> EmitAs b) -> a -> EmitAs YamlNode
asYAMLwith f x = do
    ptr  <- liftIO $ addressOf x
    seen <- ask
    case IntMap.lookup ptr seen of
        Just node   -> return node
        _           -> mdo
            rv   <- local (IntMap.insert ptr rv) (asYAML =<< f x)
            return rv

addressOf :: a -> IO Int
addressOf x = do
    ptr <- newStablePtr x
    return (castStablePtrToPtr ptr `minusPtr` (nullPtr :: Ptr ()))
