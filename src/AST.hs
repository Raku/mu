{-# OPTIONS -fglasgow-exts #-}

{-
    Abstract syntax tree.

    Tall ships and tall kings
    Three times three.
    What brought they from the foundered land
    Over the flowing sea?
    Seven stars and seven stones
    And one white tree.
-}

module AST where
import Internals

type Ident = String

instance Show (a -> b) where
    show f = "sub { ... }"

class Context n where
    vCast :: Val -> n
    vCast (VRef v)      = vCast v
    vCast (VPair _ v)   = vCast v
    vCast v             = doCast v
    doCast :: Val -> n
    doCast v = error $ "cannot cast: " ++ (show v)

instance Context VBool where
    doCast (VJunc j l) = juncToBool j l
    doCast (VBool b)   = b
    doCast VUndef      = False
    doCast (VStr "")   = False
    doCast (VStr "0")  = False
    doCast (VInt 0)    = False
    doCast (VRat 0)    = False
    doCast (VNum 0)    = False
    doCast (VList [])  = False
    doCast _           = True

juncToBool :: JuncType -> [Val] -> Bool
juncToBool JAny     = any vCast
juncToBool JAll     = all vCast
juncToBool JNone    = all (not . vCast)
juncToBool JOne     = (1 ==) . length . filter vCast

instance Context VInt where
    doCast (VInt i)     = i
    doCast (VStr s)
        | ((n, _):_) <- reads s = n
        | otherwise             = 0
    doCast x            = round (vCast x :: VNum)

instance Context VRat where
    doCast (VInt i)     = i % 1
    doCast (VRat r)     = r
    doCast x            = approxRational (vCast x :: VNum) 1

instance Context VNum where
    doCast VUndef       = 0
    doCast (VBool b)    = if b then 1 else 0
    doCast (VInt i)     = fromIntegral i
    doCast (VRat r)     = realToFrac r
    doCast (VNum n)     = n
    doCast (VStr s)
        | ((n, _):_) <- reads s = n
        | otherwise             = 0
    doCast (VList l)    = fromIntegral $ length l
    doCast x            = error $ "cannot cast: " ++ (show x)

instance Context VComplex where
    doCast x            = (vCast x :: VNum) :+ 0

instance Context VStr where
    vCast VUndef        = ""
    vCast (VStr s)      = s
    vCast (VBool b)     = if b then "1" else "0"
    vCast (VInt i)      = show i
    vCast (VRat r)      = showNum $ realToFrac r
    vCast (VNum n)      = showNum n
    vCast (VList l)     = unwords $ map vCast l
    vCast (VRef v)      = vCast v
    vCast (VPair k v)   = vCast k ++ "\t" ++ vCast v ++ "\n"
    doCast x            = error $ "cannot cast: " ++ (show x)

showNum x
    | (i, ".0") <- break (== '.') str
    = i -- strip the trailing ".0"
    | otherwise = str
    where
    str = show x 

instance Context VList where
    vCast (VList l)     = l
    vCast (VPair k v)   = [k, v]
    vCast (VRef v)      = vCast v
    vCast v             = [v]

instance Context (Maybe a) where
    vCast VUndef        = Nothing
    vCast _             = Just undefined

instance Context Int   where doCast = intCast
instance Context Word  where doCast = intCast
instance Context Word8 where doCast = intCast
instance Context [Word8] where doCast = map (toEnum . ord) . vCast

type VScalar = Val

instance Context VScalar where
    vCast x             = x

strRangeInf s = (s:strRangeInf (strInc s))

strRange s1 s2
    | s1 == s2              = [s2]
    | length s1 > length s2 = []
    | otherwise             = (s1:strRange (strInc s1) s2)

strInc []       = "1"
strInc "z"      = "aa"
strInc "Z"      = "AA"
strInc "9"      = "10"
strInc str
    | x == 'z'  = strInc xs ++ "a"
    | x == 'Z'  = strInc xs ++ "A"
    | x == '9'  = strInc xs ++ "0"
    | otherwise = xs ++ [charInc x]
    where
    x   = last str
    xs  = init str

charInc x   = chr $ 1 + ord x

intCast x   = fromIntegral (vCast x :: VInt)

type VBool = Bool
type VInt  = Integer
type VRat  = Rational
type VNum  = Double
type VComplex = Complex VNum
type VStr  = String
type VList = [Val]
newtype VArray = MkArray [Val] deriving (Show, Eq, Ord)
newtype VHash  = MkHash (FiniteMap Val Val) deriving (Show, Eq, Ord)

instance (Show a, Show b) => Show (FiniteMap a b) where
    show fm = show (fmToList fm)

instance (Ord a, Ord b) => Ord (FiniteMap a b) where

instance Ord VComplex where
    {- ... -}

data Val
    = VUndef
    | VBool     VBool
    | VInt      VInt
    | VRat      VRat
    | VNum      VNum
    | VComplex  VComplex
    | VStr      VStr
    | VList     VList
    | VArray    VArray
    | VHash     VHash
    | VRef      Val
    | VPair     Val Val
    | VSub      Exp
    | VBlock    Exp
    | VJunc     JuncType [Val]
    | VError    VStr Exp
    | VPoly     { polyScalar    :: Val
                , polyList      :: Val
                }
    deriving (Show, Eq, Ord)

data Trait
    = TScalar   Val
    | TArray    Val
    | THash     Val

data JuncType = JAll | JAny | JOne | JNone
    deriving (Show, Eq, Ord)

data Exp
    = Op1 String Exp
    | Op2 String Exp Exp
    | Op3 String Exp Exp Exp
    | OpCmp String Exp Exp
    | Val Val
    | NonTerm SourcePos
    deriving (Show, Eq, Ord)
