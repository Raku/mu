{-# OPTIONS_GHC -fglasgow-exts -funbox-strict-fields -fallow-overlapping-instances -fvia-C #-}

module DrIFT.YAML where
import Data.Yaml.Syck
import Data.Ratio
import Data.List (intersperse)
import Data.Char (chr)
import GHC.Exts
import UTF8

type YAMLClass = String
type YAMLKey = String
type YAMLVal = YamlNode

showYaml :: YAML a => a -> IO String
showYaml x = do
    rv <- emitYaml =<< asYAML x
    case rv of
        Left e  -> error e
        Right s -> return s

class YAML a where
    asYAML :: a -> IO YamlNode
    asYAML _ = return $ nilNode

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

instance YAML ()

instance YAML Int where
    asYAML x = return $ mkTagNode "int" (YamlStr $ show x)

instance YAML String where
    asYAML str = return $ mkTagNode "str" (YamlStr str)

instance YAML Bool where
    asYAML True = return $ mkTagNode "bool#yes" (YamlStr "1")
    asYAML False = return $ mkTagNode "bool#no" (YamlStr "0")

instance YAML Integer where 
    asYAML x = return $ mkTagNode "int" (YamlStr $ show x)
instance YAML Rational where 
    asYAML r = asYAML (x, y)
        where
        x = numerator r
        y = denominator r
instance YAML Double where 
    asYAML num | show num == "Infinity"  = return $ mkTagNode "float#inf"    (YamlStr ".Inf")
               | show num == "-Infinity" = return $ mkTagNode "float#neginf" (YamlStr "-.Inf")
               | show num == "NaN"       = return $ mkTagNode "float#nan"    (YamlStr "-.NaN")
               | otherwise               = return $ mkTagNode "float"        (YamlStr $ show num)

instance (YAML a) => YAML (Maybe a) where
    asYAML (Just x) = asYAML x
    asYAML Nothing = return $ nilNode

instance (YAML a) => YAML [a] where
    asYAML xs = do
        xs' <- mapM asYAML xs
        (return . mkNode . YamlSeq) xs'

instance (YAML a, YAML b) => YAML (a, b) where
    asYAML (x, y) = do
        x' <- asYAML x
        y' <- asYAML y
        return $ mkNode (YamlSeq [x', y'])

instance (YAML a, YAML b, YAML c) => YAML (a, b, c) where
    asYAML (x, y, z) = do
        x' <- asYAML x
        y' <- asYAML y
        z' <- asYAML z
        return $ mkNode (YamlSeq [x', y', z'])

