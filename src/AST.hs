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
import Context
import Rule

type Ident = String

class Value n where
    vCast :: Val -> n
    -- vCast (MVal v)      = vCast $ castV v
    vCast (VRef v)      = vCast v
    vCast (VPair (_, v))   = vCast v
    vCast (VArray (MkArray v))    = vCast $ VList v
    vCast v             = doCast v
    castV :: n -> Val
    castV v = error $ "cannot cast into Val"
    doCast :: Val -> n
    doCast v = error $ "cannot cast from Val: " ++ (show v)
    fmapVal :: (n -> n) -> Val -> Val
    fmapVal f = castV . f . vCast

instance Value VPair where
    castV (x, y)        = VPair (x, y)
    vCast (VPair (x, y))   = (x, y)
    vCast (VRef v)      = vCast v
    -- vCast (MVal v)      = vCast $ castV v
    vCast v             = case vCast v of
        [x, y]  -> (x, y)
        other   -> error $ "cannot cast into VPair: " ++ (show v)

instance Value VHash where
    castV = VHash
    vCast (VHash h) = h
    vCast VUndef = MkHash emptyFM
    vCast v = MkHash $ vCast v

instance Value (FiniteMap Val Val) where
    vCast (VHash (MkHash h)) = h
    vCast VUndef = emptyFM
    vCast (VPair p) = listToFM [p]
    vCast x = listToFM $ vCast x

instance Value [VPair] where
    vCast VUndef = []
    vCast (VHash (MkHash h)) = fmToList h
    vCast (VPair p) = [p]
    vCast (VList vs) =
        let fromList [] = []
            fromList ((VPair (k, v)):xs) = (k, v):fromList xs
            fromList (k:v:xs) = (k, v):fromList xs
            fromList [k] = [(k, VUndef)] -- XXX warning?
        in fromList vs
    vCast x = error $ "cannot cast into [VPair]: " ++ (show x)

instance Value VSub where
    castV = VSub
    doCast (VSub b) = b
    doCast (VList [VSub b]) = b -- XXX Wrong
    doCast v = error ("Cannot cast into VSub: " ++ (show v))

instance Value VBool where
    castV = VBool
    doCast (VJunc j)   = juncToBool j
    doCast (VBool b)   = b
    doCast VUndef      = False
    doCast (VStr "")   = False
    doCast (VStr "0")  = False
    doCast (VInt 0)    = False
    doCast (VRat 0)    = False
    doCast (VNum 0)    = False
    doCast (VList [])  = False
    doCast _           = True

juncToBool :: VJunc -> Bool
juncToBool (Junc JAny  _  vs) = (True `elementOf`) $ mapSet vCast vs
juncToBool (Junc JAll  _  vs) = not . (False `elementOf`) $ mapSet vCast vs
juncToBool (Junc JNone _  vs) = not . (True `elementOf`) $ mapSet vCast vs
juncToBool (Junc JOne  ds vs)
    | (True `elementOf`) $ mapSet vCast ds
    = False
    | otherwise
    = (1 ==) . length . filter vCast $ setToList vs

readMVal (MVal mv) = readMVal =<< liftIO (readIORef mv)
readMVal v         = return v

instance Value VInt where
    castV = VInt
    doCast (VInt i)     = i
    doCast (VStr s)
        | ((n, _):_) <- reads s = n
        | otherwise             = 0
    doCast x            = round (vCast x :: VNum)

instance Value VRat where
    castV = VRat
    doCast (VInt i)     = i % 1
    doCast (VRat r)     = r
    doCast (VBool b)    = if b then 1 % 1 else 0 % 1
    doCast (VList l)    = genericLength l
    doCast (VArray (MkArray a))    = genericLength a
    doCast (VHash (MkHash h))    = fromIntegral $ sizeFM h
    doCast x            = toRational (vCast x :: VNum)

instance Value VNum where
    castV = VNum
    doCast VUndef       = 0
    doCast (VBool b)    = if b then 1 else 0
    doCast (VInt i)     = fromIntegral i
    doCast (VRat r)     = realToFrac r
    doCast (VNum n)     = n
    doCast (VStr s)
        | ((n, _):_) <- reads s = n
        | otherwise             = 0
    doCast (VList l)    = genericLength l
    doCast (VArray (MkArray a))    = genericLength a
    doCast (VHash (MkHash h))    = fromIntegral $ sizeFM h
    doCast x            = 0/0 -- error $ "cannot cast as Num: " ++ (show x)

instance Value VComplex where
    castV = VComplex
    doCast x            = (vCast x :: VNum) :+ 0

instance Value VStr where
    castV = VStr
    vCast VUndef        = ""
    vCast (VStr s)      = s
    vCast (VBool b)     = if b then "1" else ""
    vCast (VInt i)      = show i
    vCast (VRat r)      = showNum $ realToFrac r
    vCast (VNum n)      = showNum n
    vCast (VList l)     = unwords $ map vCast l
    vCast (VRef v)      = vCast v
    -- vCast (MVal v)      = vCast $ castV v
    vCast (VPair (k, v))= vCast k ++ "\t" ++ vCast v ++ "\n"
    vCast (VArray (MkArray l))     = unwords $ map vCast l
    vCast (VSub s)      = "<" ++ show (subType s) ++ "(" ++ subName s ++ ")>"
    vCast x             = error $ "cannot cast as Str: " ++ (show x)

showNum x
    | (i, ".0") <- break (== '.') str
    = i -- strip the trailing ".0"
    | otherwise = str
    where
    str = show x 

instance Value VArray where
    castV = VArray
    vCast x = MkArray (vCast x) 

instance Value MVal where
    castV ref = error "bye~" --unsafePerformIO $ readIORef ref
    vCast (MVal x)      = x
    vCast (VRef v)      = vCast v
    vCast (VPair (_, y))= vCast y
    vCast x             = error $ "cannot modify a constant item: " ++ show x

{-
instance Value VJunc where
    castV = JAny . castV
    vCast x = JAny $ mkSet (vCast x)
-}

instance Value VList where
    castV = VList
    vCast (VList l)     = l
    vCast (VArray (MkArray l)) = l
    vCast (VHash (MkHash h)) = map VPair $ fmToList h
    vCast (VPair (k, v))   = [k, v]
    vCast (VRef v)      = vCast v
    -- vCast (MVal v)      = vCast $ castV v
    vCast (VUndef)      = []
    vCast v             = [v]

instance Value VHandle where
    castV = VHandle
    doCast (VHandle x) = x
    doCast x            = error $ "cannot cast into a handle: " ++ show x

instance Value (Maybe a) where
    vCast VUndef        = Nothing
    vCast _             = Just undefined

instance Value Int   where doCast = intCast
instance Value Word  where doCast = intCast
instance Value Word8 where doCast = intCast
instance Value [Word8] where doCast = map (toEnum . ord) . vCast

type VScalar = Val
-- type VJunc = Set Val

instance Value VScalar where
    vCast = id
    castV = id -- XXX not really correct; need to referencify things

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
type VHandle = Handle
type MVal = IORef Val
newtype VArray = MkArray [Val] deriving (Show, Eq, Ord)
newtype VHash  = MkHash (FiniteMap Val Val) deriving (Show, Eq, Ord)

type VPair = (Val, Val)

instance (Show a, Show b) => Show (FiniteMap a b) where
    show fm = show (fmToList fm)

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
    | VPair     VPair
    | VSub      VSub
    | VBlock    VBlock
    | VJunc     VJunc
    | VError    VStr Exp
    | VHandle   VHandle
    | MVal      MVal
    | VControl  VControl
    deriving (Show, Eq, Ord)

valType VUndef          = "Any"
valType (VRef v)        = valType v
valType (VBool    _)    = "Bool"
valType (VInt     _)    = "Int"
valType (VRat     _)    = "Rat"
valType (VNum     _)    = "Num"
valType (VComplex _)    = "Complex"
valType (VStr     _)    = "Str"
valType (VList    _)    = "List"
valType (VArray   _)    = "Array"
valType (VHash    _)    = "Hash"
valType (VPair    _)    = "Pair"
valType (VSub     _)    = "Sub"
valType (VBlock   _)    = "Block"
valType (VJunc    _)    = "Junc"
valType (VError _ _)    = "Error"
valType (VHandle  _)    = "Handle"
valType (MVal     _)    = "Var"
valType (VControl _)    = "Control"

type VBlock = Exp
data VControl
    = ControlLeave (Env -> Eval Bool) Val
    | ControlExit ExitCode
    deriving (Show, Eq, Ord)

data VJunc = Junc { juncType :: JuncType
                  , juncDup  :: Set Val
                  , juncSet  :: Set Val
                  } deriving (Show, Eq, Ord)

data JuncType = JAny | JAll | JNone | JOne
    deriving (Show, Eq, Ord)

data SubType = SubMethod | SubRoutine | SubBlock | SubPrim
    deriving (Show, Eq, Ord)

data Param = Param
    { isInvocant    :: Bool
    , isSlurpy      :: Bool
    , isOptional    :: Bool
    , isNamed       :: Bool
    , isLValue      :: Bool
    , paramName     :: String
    , paramContext  :: Cxt
    , paramDefault  :: Exp
    }
    deriving (Show, Eq, Ord)

type Params = [Param]

data VSub = Sub
    { isMulti       :: Bool
    , subName       :: String
    , subType       :: SubType
    , subPad        :: Pad
    , subAssoc      :: String
    , subParams     :: Params
    , subReturns    :: Cxt
    , subFun        :: Exp
    }
    deriving (Show, Eq, Ord)

instance (Ord a) => Ord (Set a) where
    compare x y = compare (setToList x) (setToList y)
instance (Show a) => Show (Set a) where
    show x = show $ setToList x
instance Ord VComplex where {- ... -}
instance (Ord a, Ord b) => Ord (FiniteMap a b)
instance Ord MVal where
    compare x y = LT -- compare (castV x) (castV y)
instance Show MVal where
    show _ = "<mval>"
instance Show (IORef Pad) where
    show _ = "<pad>"
instance Ord VHandle where
    compare x y = compare (show x) (show y)

type Var = String
-- type MVal = IORef Val

data Exp
    = App String [Exp] [Exp]
    | Syn String [Exp]
    | Sym Symbol
    | Prim ([Val] -> Eval Val)
--  | MVal MVal
    | Val Val
    | Var Var
    | Parens Exp
    | NonTerm SourcePos
    | Parser (CharParser Env Exp)
    deriving (Show, Eq, Ord)

instance Show (CharParser Env Exp) where
    show _ = "<parser>"
instance Eq (CharParser Env Exp)
instance Ord (CharParser Env Exp) where
    compare _ _ = LT

extractExp :: Exp -> ([Exp], [String]) -> ([Exp], [String])
extractExp exp (exps, vs) = (exp':exps, vs')
    where
    (exp', vs') = extract (exp, vs)

extract :: (Exp, [String]) -> (Exp, [String])
extract ((App n invs args), vs) = (App n invs' args', vs'')
    where
    (invs', vs')  = foldr extractExp ([], vs) invs
    (args', vs'') = foldr extractExp ([], vs') args
extract ((Syn n exps), vs) = (Syn n exps', vs')
    where
    (exps', vs') = foldr extractExp ([], vs) exps
extract ((Var name), vs)
    | (sigil:'^':identifer) <- name
    , name' <- (sigil : identifer)
    = (Var name', insert name' vs)
    | name == "$_"
    = (Var name, insert name vs)
    | otherwise
    = (Var name, vs)
extract ((Parens exp), vs) = ((Parens exp'), vs')
    where
    (exp', vs') = extract (exp, vs)
extract other = other

cxtOfSigil '$'  = "Scalar"
cxtOfSigil '@'  = "Array"
cxtOfSigil '%'  = "Hash"
cxtOfSigil '&'  = "Code"

--- cxtOf '*' '$'   = "List"
cxtOf '*' '@'   = "List"
cxtOf _   _     = "Scalar"

buildParam cxt sigil name exp = Param
    { isInvocant    = False
    , isSlurpy      = (sigil == "*")
    , isOptional    = (sigil ==) `any` ["?", "+"]
    , isNamed       = (null sigil || head sigil /= '+')
    , isLValue      = False
    , paramName     = name
    , paramContext  = if null cxt then defaultCxt else cxt
    , paramDefault  = exp
    }
    where
    sig = if null sigil then ' ' else head sigil
    defaultCxt = cxtOf sig (head name) 

defaultArrayParam   = buildParam "" "*" "@_" (Val VUndef)
defaultHashParam    = buildParam "" "*" "%_" (Val VUndef)
defaultScalarParam  = buildParam "" "*" "$_" (Val VUndef)

data Env = Env { envContext :: Cxt
	           , envLValue  :: Bool
               , envLexical :: Pad
               , envGlobal  :: IORef Pad
               , envClasses :: ClassTree
               , envEval    :: Exp -> Eval Val
               , envCaller  :: Maybe Env
               , envBody    :: Exp
               , envDepth   :: Int
               , envID      :: Unique
               , envDebug   :: Maybe (IORef (FiniteMap String String))
               } deriving (Show, Eq)

type Pad = [Symbol]
data Symbol = Symbol { symScope :: Scope
                     , symName  :: String
                     , symExp   :: Exp
                     } deriving (Show, Eq, Ord)

data Scope = SGlobal | SMy | SOur | SLet | STemp | SState
    deriving (Show, Eq, Ord, Read, Enum)

type Eval x = ContT Val (ReaderT Env IO) x
