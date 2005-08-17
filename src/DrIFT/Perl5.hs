{-# OPTIONS_GHC -fglasgow-exts -funbox-strict-fields -fallow-overlapping-instances #-}

module DrIFT.Perl5 where
import Data.Ratio
import Data.List (intersperse)

type Perl5Class = String
type Perl5Key = String
type Perl5Val = String

class (Show a) => Perl5 a where
    showPerl5 :: a -> String
    showPerl5 x = show (show x)

showP5ArrayObj :: Perl5Class -> [Perl5Val] -> String
showP5ArrayObj = showP5Obj showP5Array

showP5HashObj :: Perl5Class -> [(Perl5Key, Perl5Val)] -> String
showP5HashObj = showP5Obj showP5Hash

showP5Array :: [Perl5Val] -> String
showP5Array xs = ('[' : (concat $ intersperse "," xs)) ++ "]"

showP5Hash :: [(Perl5Key, Perl5Val)] -> String
showP5Hash xs = ('{' : (concat $ intersperse "," (map showPair xs))) ++ "}"
    where
    showPair (k, v) = k ++ " => " ++ v

showP5Class :: Perl5Class -> String
showP5Class = show

showP5Obj :: (a -> String) -> Perl5Class -> a -> String
showP5Obj f cls dat = "bless(" ++ f dat ++ " => " ++ showP5Class cls ++ ")"

-- XXX - overlapping instances?
instance Perl5 () where
    showPerl5 _ = "undef"

instance Perl5 Int where
    showPerl5 = show

instance Perl5 String where
    showPerl5 str = "\"" ++ concatMap escape str ++ "\""
        where
        escape '\\' = "\\\\"
        escape '"'  = "\\\""
        escape '$'  = "\\$"
        escape '@'  = "\\@"
        escape '%'  = "\\%"
        escape x    = x:""

instance Perl5 Bool where
    showPerl5 True = "1"
    showPerl5 False = "0"

instance Perl5 Integer where 
    showPerl5 = show
instance Perl5 Rational where 
    showPerl5 r = "(" ++ show x ++ "/" ++ show y ++ ")"
        where
        x = numerator r
        y = denominator r
instance Perl5 Double where 
    showPerl5 num | show num == "Infinity"  = "Math::BigInt->binf"
                  | show num == "-Infinity" = "Math::BigInt->binf('-')"
                  | show num == "NaN"       = "Math::BigInt->bnan"
                  | otherwise               = show num

instance (Perl5 a) => Perl5 (Maybe a) where
    showPerl5 (Just x) = showPerl5 x
    showPerl5 Nothing = "(undef)"

instance (Perl5 a) => Perl5 [a] where
    showPerl5 = showP5Array . map showPerl5

instance (Perl5 a, Perl5 b) => Perl5 (a, b) where
    showPerl5 (x, y) = showP5Array [showPerl5 x, showPerl5 y]

instance (Perl5 a, Perl5 b, Perl5 c) => Perl5 (a, b, c) where
    showPerl5 (x, y, z) = showP5Array [showPerl5 x, showPerl5 y, showPerl5 z]

