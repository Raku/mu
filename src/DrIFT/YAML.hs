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
    rv <- emitYaml (asYAML x)
    case rv of
        Left e  -> error e
        Right s -> return s

class (Show a) => YAML a where
    asYAML :: a -> YamlNode

asYAMLseq :: YAMLClass -> [YAMLVal] -> YamlNode
asYAMLseq c ps = mkTagNode (tagHs c) (YamlSeq ps)

asYAMLmap :: YAMLClass -> [(YAMLKey, YAMLVal)] -> YamlNode
asYAMLmap c ps = mkTagNode (tagHs c) (YamlMap [ (asYAML k, v) | (k, v) <- ps ])

asYAMLcls :: YAMLClass -> YamlNode
asYAMLcls c = mkTagNode (tagHs c) (YamlStr c)

tagHs :: YAMLClass -> String
tagHs = ("tag:hs:" ++)

-- XXX - overlapping instances?
instance YAML () where
    asYAML _ = nilNode

instance YAML Int where
    asYAML x = mkTagNode "int" (YamlStr $ show x)

instance YAML String where
    asYAML str = mkTagNode "str" (YamlStr str)

instance YAML Bool where
    asYAML True = mkTagNode "bool#yes" (YamlStr "1")
    asYAML False = mkTagNode "bool#no" (YamlStr "0")

instance YAML Integer where 
    asYAML x = mkTagNode "int" (YamlStr $ show x)
instance YAML Rational where 
    asYAML r = asYAML (x, y)
        where
        x = numerator r
        y = denominator r
instance YAML Double where 
    asYAML num | show num == "Infinity"  = mkTagNode "float#inf"    (YamlStr ".Inf")
               | show num == "-Infinity" = mkTagNode "float#neginf" (YamlStr "-.Inf")
               | show num == "NaN"       = mkTagNode "float#nan"    (YamlStr "-.NaN")
               | otherwise               = mkTagNode "float"        (YamlStr $ show num)

instance (YAML a) => YAML (Maybe a) where
    asYAML (Just x) = asYAML x
    asYAML Nothing = nilNode

instance (YAML a) => YAML [a] where
    asYAML = mkNode . YamlSeq . map asYAML

instance (YAML a, YAML b) => YAML (a, b) where
    asYAML (x, y) = mkNode (YamlSeq [asYAML x, asYAML y])

instance (YAML a, YAML b, YAML c) => YAML (a, b, c) where
    asYAML (x, y, z) = mkNode (YamlSeq [asYAML x, asYAML y, asYAML z])

