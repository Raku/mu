module Text.Parser.Rule where
import Prelude hiding (length, lookup, null, drop, span)
import Text.Parser.OpTable
import Text.Parser.PArrow
import Data.FastPackedString hiding (concatMap, concat)
import Data.Char (isAlphaNum)
import Control.Arrow

main :: IO ()
main = print (parse "1+|2*" "12")

parse :: String -> String -> Either (Int, [String]) String
parse rule input = runParser (comp $ opParse ruleTable (pack rule)) (pack input)

comp :: Rule -> MD i String
comp (Lit x) = string (unpack x)
comp (Star x) = many (comp x) >>^ concat
comp (Plus x) = many1 (comp x) >>^ concat
comp (Concat x y) = comp x >>> comp y
comp (Altern x y) = choice [comp x, comp y]

data Rule
    = Lit !FastString
    | Star !Rule
    | Plus !Rule
    | Concat !Rule !Rule
    | Altern !Rule !Rule
    deriving (Eq, Show, Ord)

ruleTable :: OpTable Rule
ruleTable = mkOpTable
    [ op _Lit Term (span isAlphaNum)
    , concatMap (\x -> op (_Quant x) Postfix x) ["*", "+"]
    , op _Concat Infix AssocRight ""
    , op _Altern Infix "|" 
    ]
    where
    _Quant :: String -> DynMkMatch Rule
    _Quant "+" _ [x] = Plus x
    _Quant "*" _ [x] = Star x
    _Quant _   _ _   = error "unknown quant"
    _Lit, _Concat, _Altern :: DynMkMatch Rule
    _Lit tok _ = Lit (tokStr tok)
    _Concat _ [x, y] = Concat x y
    _Altern _ [x, y] = Altern x y
