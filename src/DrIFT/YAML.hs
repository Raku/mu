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

type YAMLClass = String
type YAMLKey = String
type YAMLVal = YamlNode

showYaml :: YAML a => a -> IO String
showYaml x = do
    node    <- asYAML x
    rv      <- emitYaml node
    case rv of
        Left e  -> error e
        Right s -> return s

class Typeable a => YAML a where
    asYAML :: a -> IO YamlNode
    asYAML x = do
        ty <- Control.Exception.handle (const $ return "()") $
            evaluate (reverse (takeWhile (/= '.') (reverse (show (typeOf x)))))
        return $ case ty of
            "()" -> nilNode
            _    -> mkTagNode (tagHs ty) YamlNil
    fromYAML:: YamlNode -> IO a
    fromYAML (MkYamlNode{el=x}) = fromYAMLElem x
    fromYAMLElem :: YamlElem -> IO a

asYAMLseq :: YAMLClass -> [IO YAMLVal] -> IO YamlNode
asYAMLseq c ps = do
    ps' <- sequence ps
    return $ mkTagNode (tagHs c) (YamlSeq ps')

asYAMLmap :: YAMLClass -> [(YAMLKey, IO YAMLVal)] -> IO YamlNode
asYAMLmap c ps = do
    ps' <- mapM asYAMLpair ps
    return $ mkTagNode (tagHs c) (YamlMap ps')
    where
    asYAMLpair (k, v) = do
        k' <- asYAML k
        v' <- v
        return (k', v')

asYAMLcls :: YAMLClass -> IO YamlNode
asYAMLcls c = return $ mkTagNode (tagHs c) (YamlStr c)

tagHs :: YAMLClass -> String
tagHs = ("tag:hs:" ++)

instance YAML () where
    asYAML _ = return nilNode
    fromYAML _ = return ()

instance YAML Int where
    asYAML x = return $ mkTagNode "int" (YamlStr $ show x)
    fromYAMLElem ~(YamlStr x) = return $ read x

instance YAML String where
    asYAML str = return $ mkTagNode "str" (YamlStr str)
    fromYAMLElem ~(YamlStr str) = return $ read str

instance YAML Bool where
    asYAML True = return $ mkTagNode "bool#yes" (YamlStr "1")
    asYAML False = return $ mkTagNode "bool#no" (YamlStr "0")
    fromYAML (MkYamlNode{tag=Just "bool#yes"}) = return True
    fromYAML (MkYamlNode{tag=Just "bool#no"})  = return False

instance YAML Integer where 
    asYAML x = return $ mkTagNode "int" (YamlStr $ show x)
    fromYAMLElem ~(YamlStr x) = return $ read x

instance YAML Rational where 
    asYAML r = asYAML (x, y)
        where
        x = numerator r
        y = denominator r
    fromYAMLElem ~(YamlStr str) = return $ (read x) / (read y)
        where
        (x,y) = break (== '/') str
    
instance YAML Double where 
    asYAML num | show num == "Infinity"  = return $ mkTagNode "float#inf"    (YamlStr ".Inf")
               | show num == "-Infinity" = return $ mkTagNode "float#neginf" (YamlStr "-.Inf")
               | show num == "NaN"       = return $ mkTagNode "float#nan"    (YamlStr "-.NaN")
               | otherwise               = return $ mkTagNode "float"        (YamlStr $ show num)
    fromYAML (MkYamlNode{tag=Just "float#inf"})    = return $  1/0 -- "Infinity" 
    fromYAML (MkYamlNode{tag=Just "float#neginf"}) = return $ -1/0 -- "-Infinity" 
    fromYAML (MkYamlNode{tag=Just "float#nan"})    = return $  0/0 -- "NaN" 
    fromYAML ~(MkYamlNode{el=YamlStr x}) = return $ read x

instance (YAML a) => YAML (Maybe a) where
    asYAML (Just x) = asYAML x
    asYAML Nothing = return $ nilNode
    fromYAML (MkYamlNode{el=YamlNil}) = return Nothing
    fromYAML x = return . Just =<< fromYAML x
    --fromYAML = fmap Just . fromYAML

instance (YAML a) => YAML [a] where
    asYAML xs = do
        xs' <- mapM asYAML xs
        (return . mkNode . YamlSeq) xs'
    fromYAML ~(MkYamlNode{el=YamlSeq s}) = mapM fromYAML s

instance (YAML a, YAML b) => YAML (a, b) where
    asYAML (x, y) = do
        x' <- asYAML x
        y' <- asYAML y
        return $ mkNode (YamlSeq [x', y'])
    fromYAML ~(MkYamlNode{el=YamlSeq [x, y]}) = liftM2 (,) (fromYAML x) (fromYAML y)

instance (YAML a, YAML b, YAML c) => YAML (a, b, c) where
    asYAML (x, y, z) = do
        x' <- asYAML x
        y' <- asYAML y
        z' <- asYAML z
        return $ mkNode (YamlSeq [x', y', z'])
    fromYAML ~(MkYamlNode{el=YamlSeq [x, y, z]}) = liftM3 (,,) (fromYAML x) (fromYAML y) (fromYAML z)

