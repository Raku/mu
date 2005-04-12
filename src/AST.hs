{-# OPTIONS_GHC -fglasgow-exts -fno-warn-orphans #-}
{-# OPTIONS_GHC -#include "UnicodeC.h" #-}

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
import List
import Types
import qualified Types.Array  as Array
import qualified Types.Handle as Handle
import qualified Types.Hash   as Hash
import qualified Types.Scalar as Scalar
import qualified Types.Code   as Code
import qualified Types.Rule   as Rule
import qualified Data.Set       as Set
import qualified Data.HashTable as HTable

type Ident = String

ifValTypeIsa v typ trueM falseM = do
    env <- ask
    vt  <- evalValType v
    if isaType (envClasses env) typ vt
        then trueM
        else falseM

ifContextIsa c trueM falseM = do
    env <- ask
    if isaType (envClasses env) c (envContext env)
        then trueM
        else falseM

evalValType (VRef r) = do
    cls <- asks envClasses
    let typ = refType r
    if isaType cls "Scalar" typ
        then evalValType =<< readRef r
        else return typ
evalValType val = return $ valType val

fromVal' (VThunk (MkThunk eval)) = fromVal' =<< eval
fromVal' (VRef r) = do
    v <- readRef r
    fromVal' v
fromVal' v = do
    rv <- liftIO $ catchJust errorCalls (return . Right $ vCast v) $
        \str -> return (Left str)
    case rv of
        Right v -> return v
        Left e  -> retError e (Val v) -- XXX: not working yet

-- fromMVal = (>>= fromVal) . readMVal

class (Typeable n) => Value n where
    fromVal :: Val -> Eval n
    fromVal = fromVal'
    vCast :: Val -> n
    -- vCast (MVal v)      = vCast $ castV v
    vCast v@(VRef _)    = castFail v
    vCast (VPair (_, v))= vCast v
--  vCast (VArray v)    = vCast $ VList $ IntMap.elems v
    vCast v             = doCast v
    castV :: n -> Val
    castV _ = error $ "cannot cast into Val"
    doCast :: Val -> n
    doCast v = error $ "cannot cast from Val: " ++ show v
    fmapVal :: (n -> n) -> Val -> Val
    fmapVal f = castV . f . vCast

castFail v = err
    where
    err = error $ "cannot cast from " ++ show v ++ " to " ++ typ
    typ = show $ typeOf err

instance Value (IVar VScalar) where
    fromVal (VRef (MkRef v@(IScalar _))) = return v
    fromVal (VRef r) = fromVal =<< readRef r
    fromVal v = return $ constScalar v

instance Value VRef where
    fromVal (VRef v) = return v
    fromVal (VList v) = return (arrayRef v)
    fromVal v = return . MkRef $ constScalar v
    castV = VRef

instance Value [Int] where
    fromVal v = do
        vlist <- fromVal v
        mapM fromVal vlist

instance Value [VStr] where
    fromVal v = do
        vlist <- fromVal v
        mapM fromVal vlist

instance Value VPair where
    castV (x, y)        = VPair (x, y)
    vCast (VPair (x, y))   = (x, y)
    -- vCast (MVal v)      = vCast $ castV v
    vCast v             = case vCast v of
        [x, y]  -> (x, y)
        _       -> castFail v

instance Value VHash where
    -- vCast (VHash h) = h
    -- vCast VUndef = emptyFM
    fromVal (VPair (k, v)) = do
        str <- fromVal k
        return [(str, v)]
    fromVal v = do
        list <- fromVal v
        doFrom list
        where
        doFrom [] = return []
        doFrom (VPair (k, v):list) = do
            str  <- fromVal k
            rest <- doFrom list
            return ((str, v):rest)
        doFrom (k:v:list) = do
            str  <- fromVal k
            rest <- doFrom list
            return ((str, v):rest)
        doFrom [k] = do
            str  <- fromVal k
            -- XXX: warn about odd elements?
            return [(str, undef)]

instance Value [VPair] where
    -- vCast VUndef = []
    -- vCast (VHash h) = [ (VStr k, v) | (k, v) <- Map.assocs h ]
    vCast (VPair p) = [p]
    -- vCast (VArray vs) = [ (castV k, v) | (k, v) <- IntMap.assocs vs ]
    vCast (VList vs) =
        let fromList [] = []
            fromList ((VPair (k, v)):xs) = (k, v):fromList xs
            fromList (k:v:xs) = (k, v):fromList xs
            fromList [k] = [(k, VUndef)] -- XXX warning?
        in fromList vs
    vCast v = castFail v

instance Value VCode where
    castV = VCode
    doCast (VCode b) = b
    doCast (VList [VCode b]) = b -- XXX Wrong
    doCast v = castFail v

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
juncToBool (Junc JAny  _  vs) = (True `Set.member`) $ Set.map vCast vs
juncToBool (Junc JAll  _  vs) = not . (False `Set.member`) $ Set.map vCast vs
juncToBool (Junc JNone _  vs) = not . (True `Set.member`) $ Set.map vCast vs
juncToBool (Junc JOne  ds vs)
    | (True `Set.member`) $ Set.map vCast ds
    = False
    | otherwise
    = (1 ==) . length . filter vCast $ Set.elems vs

instance Value VInt where
    castV = VInt
    doCast (VInt i)     = i
    doCast x            = truncate (vCast x :: VRat)

instance Value VRat where
    castV = VRat
    doCast (VInt i)     = i % 1
    doCast (VRat r)     = r
    doCast (VBool b)    = if b then 1 % 1 else 0 % 1
    doCast (VList l)    = genericLength l
    -- doCast (VArray a)   = toRational $ IntMap.size a
    -- doCast (VHash h)    = fromIntegral $ Map.size h
    doCast (VStr s) | not (null s) , isSpace $ last s = doCast (VStr $ init s)
    doCast (VStr s) | not (null s) , isSpace $ head s = doCast (VStr $ tail s)
    doCast (VStr s)     =
        case ( runParser naturalOrRat () "" s ) of
            Left _   -> 0 % 1
            Right rv -> case rv of
                Left  i -> i % 1
                Right d -> d
    doCast x            = toRational (vCast x :: VNum)

instance Value VNum where
    castV = VNum
    doCast VUndef       = 0
    doCast (VBool b)    = if b then 1 else 0
    doCast (VInt i)     = fromIntegral i
    doCast (VRat r)     = realToFrac r
    doCast (VNum n)     = n
    doCast (VStr s) | not (null s) , isSpace $ last s = doCast (VStr $ init s)
    doCast (VStr s) | not (null s) , isSpace $ head s = doCast (VStr $ tail s)
    doCast (VStr "Inf") = 1/0
    doCast (VStr "NaN") = 0/0
    doCast (VStr s)     =
        case ( runParser naturalOrRat () "" s ) of
            Left _   -> 0
            Right rv -> case rv of
                Left  i -> fromIntegral i
                Right d -> realToFrac d
    doCast (VList l)    = genericLength l
--  doCast (VArray a)   = fromIntegral $ IntMap.size a
--  doCast (VHash h)    = fromIntegral $ Map.size h
    doCast t@(VThread _)  = read $ vCast t
    doCast _            = 0/0 -- error $ "cannot cast as Num: " ++ show x

instance Value VComplex where
    castV = VComplex
    doCast x            = (vCast x :: VNum) :+ 0

instance Value VStr where
    castV = VStr
{-
    fromVal (VHash h) = do
        ls <- mapM strPair $ Map.assocs h
        return $ unlines ls
        where
        strPair (k, v) = do
            -- k' <- fromMVal k
            v' <- fromMVal v
            return $ k ++ "\t" ++ v'
-}
    fromVal (VList l)   = return . unwords =<< mapM fromVal l
    fromVal v = fromVal' v
    vCast VUndef        = ""
    vCast (VStr s)      = s
    vCast (VBool b)     = if b then "1" else ""
    vCast (VInt i)      = show i
    vCast (VRat r)
        | frac == 0 = s ++ show quot
        | otherwise = s ++ show quot ++ "." ++ showFrac frac
        where
        n = numerator r
        d = denominator r
        s = if signum n < 0 then "-" else ""
        (quot, rem) = quotRem (abs n) d
        frac :: VInt
        frac = round ((rem * (10 ^ (40 :: VInt))) % d)
        showFrac = reverse . dropWhile (== '0') . reverse . pad . show
        pad x = (replicate (40 - length x) '0') ++ x
    vCast (VNum n)      = showNum n
    vCast (VList l)     = unwords $ map vCast l
    -- vCast (MVal v)      = vCast $ castV v
    vCast (VPair (k, v))= vCast k ++ "\t" ++ vCast v ++ "\n"
    -- vCast (VArray l)    = unwords . map vCast $ IntMap.elems l
    -- vCast (VHash h)     = unlines $
    --     map (\(k, v) -> (k ++ "\t" ++ vCast v)) $ Map.assocs h
    vCast (VCode s)      = "<" ++ show (subType s) ++ "(" ++ subName s ++ ")>"
    vCast (VJunc j)     = show j
    vCast (VThread t)   = dropWhile (not . isDigit) $ show t
    vCast x             = castFail x

showNum :: Show a => a -> String
showNum x
    | (i, ".0") <- break (== '.') str
    = i -- strip the trailing ".0"
    | otherwise = str
    where
    str = show x 

valToStr :: Val -> Eval VStr
valToStr = fromVal

{-
instance Value VArray where
    -- castV = VArray
    vCast x = IntMap.fromAscList $ [0..] `zip` vCast x
-}

{-
instance Value VJunc where
    castV = JAny . castV
    vCast x = JAny $ mkSet (vCast x)
-}

instance Value VList where
    fromVal (VRef r) = do
        v <- readRef r
        case v of
            (VList vs) -> return vs
            _          -> return [v]
    fromVal v = fromVal' v
    castV = VList
    vCast (VList l)     = l
    -- vCast (VArray l)    = IntMap.elems l
    -- vCast (VHash h)     = [ VPair (VStr k, v) | (k, v) <- Map.assocs h ]
    vCast (VPair (k, v))   = [k, v]
    -- vCast (MVal v)      = vCast $ castV v
    vCast (VUndef)      = [VUndef]
    vCast v             = [v]

instance Value VHandle where
    castV = VHandle
    doCast (VHandle x)  = x
    doCast x            = castFail x

instance Value VSocket where
    castV = VSocket
    doCast (VSocket x)  = x
    doCast x            = castFail x

instance Value VThread where
    castV = VThread
    doCast (VThread x)  = x
    doCast x            = castFail x

instance Value Int   where
    doCast = intCast
    castV = VInt . fromIntegral
instance Value Word  where doCast = intCast
instance Value Word8 where doCast = intCast
instance Value [Word8] where doCast = map (toEnum . ord) . vCast

type VScalar = Val
-- type VJunc = Set Val

instance Value VScalar where
    -- fromVal (VList v) = return . VRef $ arrayRef v
    fromVal (VRef r) = readRef r
    fromVal v = return v
    vCast = id
    castV = id -- XXX not really correct; need to referencify things

strRangeInf :: String -> [String]
strRangeInf s = (s:strRangeInf (strInc s))

strRange :: String -> String -> [String]
strRange s1 s2
    | s1 == s2              = [s2]
    | length s1 > length s2 = []
    | otherwise             = (s1:strRange (strInc s1) s2)

strInc :: String -> String
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

charInc :: Char -> Char
charInc x   = chr $ 1 + ord x

intCast :: Num b => Val -> b
intCast x   = fromIntegral (vCast x :: VInt)

type VList = [Val]
type VSubst = (VRule, Exp)
type VArray = [Val]
type VHash = [(VStr, Val)]
newtype VThunk = MkThunk (Eval Val)

type VPair = (Val, Val)

data Val
    = VUndef
    | VBool     VBool
    | VInt      VInt
    | VRat      VRat
    | VNum      VNum
    | VComplex  VComplex
    | VStr      VStr
    | VList     VList
    | VRef      VRef
    | VPair     VPair
    | VCode      VCode
    | VBlock    VBlock
    | VJunc     VJunc
    | VError    VStr Exp
    | VHandle   VHandle
    | VSocket   VSocket
    | VThread   VThread
    | VRule     VRule
    | VSubst    VSubst
    | VControl  VControl
    | VThunk    VThunk
    deriving (Show, Eq, Ord, Typeable)

valType :: Val -> String
valType VUndef          = "Any"
valType (VRef v)        = refType v
valType (VBool    _)    = "Bool"
valType (VInt     _)    = "Int"
valType (VRat     _)    = "Rat"
valType (VNum     _)    = "Num"
valType (VComplex _)    = "Complex"
valType (VStr     _)    = "Str"
valType (VList    _)    = "List"
valType (VPair    _)    = "Pair"
valType (VCode     _)   = "Sub"
valType (VBlock   _)    = "Block"
valType (VJunc    _)    = "Junction"
valType (VError _ _)    = "Error"
valType (VHandle  _)    = "IO"
valType (VSocket  _)    = "Socket"
valType (VThread  _)    = "Thread"
valType (VControl _)    = "Control"
valType (VThunk   _)    = "Thunk"
valType (VRule    _)    = "Rule"
valType (VSubst   _)    = "Subst"

type VBlock = Exp
data VControl
    = ControlLeave (Env -> Eval Bool) Val
    | ControlExit ExitCode
    deriving (Show, Eq, Ord)

data VJunc = Junc { juncType :: JuncType
                  , juncDup  :: Set Val
                  , juncSet  :: Set Val
                  } deriving (Eq, Ord)

data JuncType = JAny | JAll | JNone | JOne
    deriving (Eq, Ord)

instance Show JuncType where
    show JAny  = "any"
    show JAll  = "all"
    show JNone = "none"
    show JOne  = "one"

instance Show VJunc where
    show (Junc jtype _ set) =
       	(show jtype) ++ "(" ++
	    (foldl (\x y ->
		if x == "" then (vCast :: Val -> VStr) y
		else x ++ "," ++ (vCast :: Val -> VStr) y)
	    "" $ Set.elems set) ++ ")"

data SubType = SubMethod | SubRoutine | SubBlock | SubPrim
    deriving (Show, Eq, Ord)

data Param = Param
    { isInvocant    :: Bool
    , isSlurpy      :: Bool
    , isOptional    :: Bool
    , isNamed       :: Bool
    , isLValue      :: Bool
    , isThunk       :: Bool
    , paramName     :: String
    , paramContext  :: Cxt
    , paramDefault  :: Exp
    }
    deriving (Show, Eq, Ord)

type Params = [Param]
type Bindings = [(Param, Exp)]

data VCode = Sub
    { isMulti       :: Bool
    , subName       :: String
    , subType       :: SubType
    , subPad        :: Pad
    , subAssoc      :: String
    , subParams     :: Params
    , subBindings   :: Bindings
    , subReturns    :: Cxt
    , subFun        :: Exp
    }
    deriving (Show, Eq, Ord, Typeable)

instance Ord VComplex where {- ... -}
instance Ord IScalar where
    compare _ _ = EQ -- compare (castV x) (castV y)
instance Show (IORef Pad) where
    show _ = "<pad>"

data Exp
    = App String [Exp] [Exp]
    | Syn String [Exp]
    | Sym [Symbol Exp]
    | Prim ([Val] -> Eval Val)
    | Val Val
    | Var Var
    | Parens Exp
    | NonTerm SourcePos
    | Statements [(Exp, SourcePos)]
    deriving (Show, Eq, Ord)

fromVals :: (Value n) => Val -> Eval [n]
fromVals v = mapM fromVal =<< fromVal v

instance Show VThunk where
    show _ = "<thunk>"
instance Eq VThunk
instance Ord VThunk where
    compare _ _ = EQ

extractExp :: Exp -> ([Exp], [String]) -> ([Exp], [String])
extractExp ex (exps, vs) = (ex':exps, vs')
    where
    (ex', vs') = extract (ex, vs)

extract :: (Exp, [String]) -> (Exp, [String])
extract ((App n invs args), vs) = (App n invs' args', vs'')
    where
    (invs', vs')  = foldr extractExp ([], vs) invs
    (args', vs'') = foldr extractExp ([], vs') args
extract ((Statements stmts), vs) = (Statements stmts', vs')
    where
    exps = map fst stmts
    poss = map snd stmts
    (exps', vs') = foldr extractExp ([], vs) exps
    stmts' = exps' `zip` poss
extract ((Syn n exps), vs) = (Syn n exps', vs'')
    where
    (exps', vs') = foldr extractExp ([], vs) exps
    vs'' = case n of
        "when"  -> nub $ vs' ++ ["$_"]
        "given" -> delete "$_" vs'
        _       -> vs'
extract ((Var name), vs)
    | (sigil:'^':identifer) <- name
    , name' <- (sigil : identifer)
    = (Var name', nub (name':vs))
    | name == "$_"
    = (Var name, nub (name:vs))
    | otherwise
    = (Var name, vs)
extract ((Parens ex), vs) = ((Parens ex'), vs')
    where
    (ex', vs') = extract (ex, vs)
extract other = other

cxtOfExp (Syn "," _) = "List"
cxtOfExp (Syn "*" _) = "List"
cxtOfExp (Syn "[]" [exp, _]) = cxtOfExp exp
cxtOfExp (Syn "{}" [exp, _]) = cxtOfExp exp
cxtOfExp (App "&infix:.." _ _) = "List"
cxtOfExp (App "&postfix:..." _ _) = "List"
cxtOfExp (Var (c:_)) = cxtOfSigil c
cxtOfExp _ = "Scalar"

cxtOfSigil :: Char -> String
cxtOfSigil '$'  = "Scalar"
cxtOfSigil '@'  = "Array"
cxtOfSigil '%'  = "Hash"
cxtOfSigil '&'  = "Code"
cxtOfSigil x    = internalError $ "cxtOfSigil: unexpected character: " ++ show x

--- cxtOf '*' '$'   = "List"
cxtOf :: Char -> Char -> String
cxtOf '*' '@'   = "List"
cxtOf _   _     = "Scalar"

buildParam :: String -> String -> String -> Exp -> Param
buildParam cxt sigil name e = Param
    { isInvocant    = False
    , isSlurpy      = (sigil == "*")
    , isOptional    = (sigil ==) `any` ["?", "+"]
    , isNamed       = (null sigil || head sigil /= '+')
    , isLValue      = False
    , isThunk       = False
    , paramName     = name
    , paramContext  = if null cxt then defaultCxt else cxt
    , paramDefault  = e
    }
    where
    sig = if null sigil then ' ' else head sigil
    defaultCxt = cxtOf sig (head name) 

defaultArrayParam :: Param
defaultHashParam :: Param
defaultScalarParam :: Param

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
               , envDebug   :: Maybe (IORef (Map String String))
               } deriving (Show, Eq)

type Pad = [Symbol VRef]
data Symbol a where
    SymVar :: Scope -> String -> VRef -> Symbol VRef
    SymExp :: Scope -> String -> Exp  -> Symbol Exp

show' :: (Show a) => a -> String
show' x = "( " ++ show x ++ " )"

instance Show (Symbol a) where
    show (SymVar s n v) = unwords [ "SymVar", show' s, show' n, show' v ]
    show (SymExp s n e) = unwords [ "SymExp", show' s, show' n, show' e ]

instance Eq (Symbol a) where
    x == y = (show x) == (show y)

instance Ord (Symbol a) where
    compare x y = compare (show x) (show y)

symScope (SymVar s _ _) = s
symScope (SymExp s _ _) = s
symName (SymVar _ n _) = n
symName (SymExp _ n _) = n
symVar (SymVar _ _ v) = v
symVar _ = error "Cannot cast SymVar to SymExp"
symExp (SymExp _ _ e) = e
symExp _ = error "Cannot cast SymExp to SymVar"

data Scope = SGlobal | SMy | SOur | SLet | STemp | SState
    deriving (Show, Eq, Ord, Read, Enum)

type Eval x = ContT Val (ReaderT Env IO) x

findSym :: String -> Pad -> Maybe VRef
findSym name pad = do
    s <- find ((== name) . symName) pad
    return $ symVar s

askGlobal :: Eval Pad
askGlobal = do
    glob <- asks envGlobal
    liftIO $ readIORef glob

writeVar :: Var -> Val -> Eval ()
writeVar name val = do
    glob <- askGlobal
    case find ((== name) . symName) glob of
        Just sym -> writeRef (symVar sym) val
        _        -> return () -- XXX Wrong

readVar :: Var -> Eval Val
readVar name = do
    glob <- askGlobal
    case find ((== name) . symName) glob of
        Just sym -> readRef $ symVar sym
        _ -> return VUndef

emptyExp = Syn "noop" []

retControl :: VControl -> Eval a
retControl c = do
    shiftT $ const (return $ VControl c)

retError :: VStr -> Exp -> Eval a
retError str exp = do
    shiftT $ const (return $ VError str exp)

naturalOrRat  = (<?> "number") $ do
    sig <- sign
    num <- natRat
    return $ if sig
        then num
        else case num of
            Left i  -> Left $ -i
            Right d -> Right $ -d
    where
    natRat = do
            char '0'
            zeroNumRat
        <|> decimalRat
                      
    zeroNumRat = do
            n <- hexadecimal <|> decimal <|> octalBad <|> octal <|> binary
            return (Left n)
        <|> decimalRat
        <|> fractRat 0
        <|> return (Left 0)                  
                      
    decimalRat = do
        n <- decimalLiteral
        option (Left n) (try $ fractRat n)

    fractRat n = do
            fract <- try fraction
            expo  <- option (1%1) expo
            return (Right $ ((n % 1) + fract) * expo) -- Right is Rat
        <|> do
            expo <- expo
            if expo < 1
                then return (Right $ (n % 1) * expo)
                else return (Right $ (n % 1) * expo)

    fraction = do
            char '.'
            try $ do { char '.'; unexpected "dotdot" } <|> return ()
            digits <- many digit <?> "fraction"
            return (digitsToRat digits)
        <?> "fraction"
        where
        digitsToRat d = digitsNum d % (10 ^ length d)
        digitsNum d = foldl (\x y -> x * 10 + (toInteger $ digitToInt y)) 0 d 

    expo :: GenParser Char st Rational
    expo = do
            oneOf "eE"
            f <- sign
            e <- decimalLiteral <?> "exponent"
            return (power (if f then e else -e))
        <?> "exponent"
        where
        power e | e < 0      = 1 % (10^abs(e))
                | otherwise  = (10^e) % 1

    -- sign            :: CharParser st (Integer -> Integer)
    sign            =   (char '-' >> return False) 
                    <|> (char '+' >> return True)
                    <|> return True

{-
    nat             = zeroNumber <|> decimalLiteral
        
    zeroNumber      = do{ char '0'
                        ; hexadecimal <|> decimal <|> octalBad <|> octal <|> decimalLiteral <|> return 0
                        }
                      <?> ""       
-}

    decimalLiteral         = number 10 digit        
    hexadecimal     = do{ char 'x'; number 16 hexDigit }
    decimal         = do{ char 'd'; number 10 digit }
    octal           = do{ char 'o'; number 8 octDigit }
    octalBad        = do{ many1 octDigit ; fail "0100 is not octal in perl6 any more, use 0o100 instead." }
    binary          = do{ char 'b'; number 2 (oneOf "01")  }

    -- number :: Integer -> CharParser st Char -> CharParser st Integer
    number base baseDigit
        = do{ digits <- many1 baseDigit
            ; let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
            ; seq n (return n)
            }          

evalExp :: Exp -> Eval Val
evalExp exp = do
    evl <- asks envEval
    evl exp

defined VUndef  = False
defined _       = True
undef = VUndef

readRef :: VRef -> Eval Val
readRef (MkRef (IScalar sv)) = Scalar.fetch sv
readRef (MkRef (ICode cv)) = do
    vsub <- Code.assuming cv [] []
    return $ VCode vsub
readRef (MkRef (IHash hv)) = do
    pairs <- Hash.fetch hv
    return $ VList $ map (\(k, v) -> VPair (castV k, v)) pairs
readRef (MkRef (IArray av)) = do
    vals <- Array.fetch av
    return $ VList vals
readRef (MkRef (IHandle io)) = return . VHandle =<< Handle.fetch io
readRef (MkRef (IRule rx)) = return . VRule =<< Rule.fetch rx

retIVar :: (Typeable a) => IVar a -> Eval Val 
retIVar = return . VRef . MkRef

writeRef :: VRef -> Val -> Eval ()
writeRef (MkRef (IScalar s)) val = Scalar.store s val
writeRef (MkRef (IArray s)) val  = Array.store s =<< fromVal val
writeRef (MkRef (IHash s)) val   = Hash.store s =<< fromVal val
writeRef (MkRef (ICode s)) val   = Code.store s =<< fromVal val
writeRef r _ = retError "cannot writeRef" (Val $ VRef r)

clearRef :: VRef -> Eval ()
clearRef (MkRef (IScalar s)) = Scalar.store s undef
clearRef (MkRef (IArray s)) = Array.clear s
clearRef (MkRef (IHash s)) = Hash.clear s
clearRef r = retError "cannot clearRef" (Val $ VRef r)

newObject "Scalar" val = liftIO $ return . scalarRef =<< newIORef val
newObject "Array" val = do
    ref <- liftIO $ return . arrayRef =<< (newIORef [] :: IO IArray)
    writeRef ref val
    return ref
newObject "Hash" val = do
    ref <- liftIO $ return . hashRef =<< (HTable.new (==) HTable.hashString :: IO IHash)
    writeRef ref val
    return ref
newObject "Code" val = do
    vcode <- fromVal val
    return $ codeRef (vcode :: VCode)
newObject cls val = retError ("Cannot create object of class " ++ cls) (Val val)

-- XXX: Refactor doHash and doArray into one -- also see Eval's [] and {}
doHash :: (Show b) => Val -> (forall a. Hash.Class a => a -> b) -> Eval b
doHash (VRef (MkRef (IHash hv))) f = return $ f hv
doHash (VRef (MkRef (IScalar sv))) f = do
    val <- Scalar.fetch sv
    case val of
        VUndef  -> do
            ref@(MkRef (IHash hv)) <- newObject "Hash" (VList [])
            Scalar.store sv (VRef ref)
            return $ f hv
        _  -> doHash val f
doHash val@(VRef _) _ = retError "Cannot cast into Hash" (Val val)
doHash val f = do
    hv  <- fromVal val
    return $ f (hv :: VHash)

doArray :: Val -> (forall a. Array.Class a => a -> b) -> Eval b
doArray (VRef (MkRef (IArray hv))) f = return $ f hv
doArray (VRef (MkRef (IScalar sv))) f = do
    val <- Scalar.fetch sv
    if defined val
        then doArray val f
        else do
            ref@(MkRef (IArray hv)) <- newObject "Array" (VList [])
            Scalar.store sv (VRef ref)
            return $ f hv
doArray val@(VRef _) _ = retError "Cannot cast into Array" (Val val)
doArray val f = do
    av  <- fromVal val
    return $ f (av :: VArray)

data (Typeable v) => IVar v where
    IScalar :: Scalar.Class a => a -> IVar VScalar
    IArray  :: Array.Class  a => a -> IVar VArray
    IHash   :: Hash.Class   a => a -> IVar VHash
    ICode   :: Code.Class   a => a -> IVar VCode
    IHandle :: Handle.Class a => a -> IVar VHandle
    IRule   :: Rule.Class   a => a -> IVar VRule

readIVar :: IVar v -> Eval v
readIVar (IScalar x) = Scalar.fetch x
readIVar _ = error "readIVar"

writeIVar :: IVar v -> v -> Eval ()
writeIVar (IScalar x) = Scalar.store x
writeIVar _ = error "writeIVar"

refType (MkRef (IScalar x)) = Scalar.iType x
refType (MkRef (IArray x))  = Array.iType x
refType (MkRef (IHash x))   = Hash.iType x
refType (MkRef (ICode x))   = Code.iType x
refType (MkRef (IHandle x)) = Handle.iType x
refType (MkRef (IRule x))   = Rule.iType x

instance Eq VRef where
    (==) = const $ const False
instance Ord VRef where
    compare _ _ = EQ
instance Show VRef where
    show v = "<" ++ refType v ++ ">"

instance Eq (IVar a) where
    (==) = const $ const False
instance Ord (IVar a) where
    compare _ _ = EQ
instance (Typeable a) => Show (IVar a) where
    show v = show (MkRef v)

scalarRef x = MkRef (IScalar x)
codeRef x   = MkRef (ICode x)
arrayRef x  = MkRef (IArray x)
hashRef x   = MkRef (IHash x)

newScalar :: (MonadIO m) => VScalar -> m (IVar VScalar)
newScalar = liftIO . (return . IScalar =<<) . newIORef

newArray :: (MonadIO m) => VArray -> m (IVar VArray)
newArray vals = liftIO $ do
    av      <- newIORef (map lazyScalar vals)
    return $ IArray av

newHandle :: (MonadIO m) => VHandle -> m (IVar VHandle)
newHandle = return . IHandle

proxyScalar :: Eval VScalar -> (VScalar -> Eval ()) -> IVar VScalar
proxyScalar fetch store = IScalar (fetch, store)

constScalar :: VScalar -> IVar VScalar
constScalar = IScalar

lazyScalar :: VScalar -> IVar VScalar
lazyScalar = IScalar . Just

lazyUndef :: IVar VScalar
lazyUndef = IScalar (Nothing :: IScalarLazy)

constArray :: VArray -> IVar VArray
constArray = IArray

instance Scalar.Class IScalarProxy where
    iType _ = "Scalar::Proxy"
    fetch = fst
    store = snd

instance Hash.Class VHash where
    iType _ = "Hash::Const"
    fetch = return
    fetchKeys = return . map fst
    fetchVal hv idx = return . maybe undef id $ lookup idx hv
    storeElem _ _ _ = retConstError undef
    deleteElem _ _ = retConstError undef

instance Array.Class VArray where
    iType _ = "Array::Const"
    store [] _ = return ()
    store _ [] = return ()
    store (a:as) vals@(v:vs) = do
        env <- ask
        ref <- fromVal a
        if isaType (envClasses env) "List" (refType ref)
            then writeRef ref (VList vals)
            else do
                writeRef ref v
                Array.store as vs
    fetch = return
    fetchSize = return . length
    fetchVal av idx = return $ head (drop idx av ++ [undef])
    storeVal _ _ _ = retConstError undef
    storeElem _ _ _ = retConstError undef

instance Hash.Class IHashEnv where
    iType _ = "Hash::Env"
    fetch _ = do
        envs <- liftIO getEnvironment
        return [ (k, VStr v) | (k, v) <- envs ]
    fetchVal _ key = tryIO undef $ do
        str <- getEnv key
        return $ VStr str
    storeVal _ key val = do
        str <- fromVal val
        liftIO $ setEnv key str True
    existsElem _ key = tryIO False $ do
        getEnv key
        return True
    deleteElem _ key = do
        liftIO $ unsetEnv key

instance Hash.Class IHash where
    fetch hv = do
        pairs <- liftIO $ HTable.toList hv
        forM pairs $ \(key, sv) -> do
            val <- readIVar sv
            return (key, val)
    fetchKeys hv = do
        pairs <- liftIO $ HTable.toList hv
        return $ map fst pairs
    fetchElem hv idx = do
        rv <- liftIO $ HTable.lookup hv idx
        case rv of
            Just sv -> return sv
            Nothing -> do
                sv <- newScalar undef
                liftIO $ HTable.insert hv idx sv
                return sv
    storeElem hv idx sv = liftIO $ HTable.insert hv idx sv
    deleteElem hv idx = liftIO $ HTable.delete hv idx
    existsElem hv idx = do
        rv <- liftIO $ HTable.lookup hv idx
        return $ isJust rv

instance Array.Class IArraySlice where
    iType _ = "Array::Slice"
    store av vals = mapM_ (uncurry writeIVar) (zip av vals)
    fetchSize = return . length
    fetchElem av idx = do
        Array.extendSize av (idx+1)
        return $ av !! idx
    storeSize _ _ = return () -- XXX error?
    storeElem _ _ _ = retConstError undef

instance Array.Class IArray where
    store av vals = do
        let svList = map lazyScalar vals
        liftIO $ writeIORef av svList
    fetchSize av = do
        svList <- liftIO $ readIORef av
        return $ length svList
    storeSize av sz = do
        liftIO . modifyIORef av $ take sz . (++ repeat lazyUndef)
    shift av = do
        svList <- liftIO $ readIORef av
        case svList of
            (sv:rest) -> do
                liftIO $ writeIORef av rest
                readIVar sv
            _ -> return undef
    unshift av vals = do
        liftIO $ modifyIORef av
            (map lazyScalar vals ++)
    extendSize _ 0 = return ()
    extendSize av sz = do
        liftIO . modifyIORef av $ \svList ->
            if null $ drop (sz-1) svList
                then take sz (svList ++ repeat lazyUndef)
                else svList
    fetchVal av idx = do
        Array.extendSize av (idx+1)
        svList <- liftIO $ readIORef av
        readIVar (svList !! idx)
    fetchKeys av = do
        svList <- liftIO $ readIORef av
        return $ zipWith const [0..] svList
    fetchElem av idx = do
        Array.extendSize av (idx+1)
        svList <- liftIO $ readIORef av
        let sv = svList !! idx
        if refType (MkRef sv) == "Scalar::Lazy"
            then do
                val <- readIVar sv
                sv' <- newScalar val
                liftIO $ writeIORef av $ take idx svList ++ (sv' : drop (idx+1) svList)
                return sv'
            else return sv
    existsElem av idx = do
        svList <- liftIO $ readIORef av
        return . not . null $ drop idx svList
    deleteElem av idx = do
        liftIO . modifyIORef av $ \svList ->
            if null $ drop (idx + 1) svList
                then take idx svList
                else take idx svList ++ (lazyUndef : drop (idx+1) svList)
    storeElem av idx sv = do
        liftIO $ modifyIORef av
            (\svList -> take idx svList ++ (sv : drop (idx+1) svList))

instance Handle.Class IHandle where
    fetch = return
    store = error "store"

instance Scalar.Class IScalar where
    fetch = liftIO . readIORef
    store = (liftIO .) . writeIORef

instance Scalar.Class IScalarLazy where
    iType _ = "Scalar::Lazy"
    fetch = return . maybe undef id
    store _ v = retConstError v

instance Scalar.Class VScalar where
    iType _ = "Scalar::Const"
    fetch = return
    store _ v = retConstError v

instance Code.Class ICode where
    fetch = liftIO . readIORef
    store = (liftIO .) . writeIORef
    assuming c [] [] = Code.fetch c
    assuming _ _ _   = undefined
    apply    = error "apply"
    assoc    = error "assoc"
    params   = error "params"

instance Code.Class VCode where
    fetch     = return
    store _ _ = retConstError undef
    assuming c [] [] = return c
    assuming _ _ _   = undefined
    apply    = error "apply"
    assoc    = subAssoc
    params   = subParams

retConstError v = retError "Can't modify constant item" (Val v)

type IArray  = IORef [IVar VScalar]
type IArraySlice = [IVar VScalar]
type IHash   = HTable.HashTable VStr (IVar VScalar)
type IScalar = IORef Val
type ICode   = IORef VCode
data IHashEnv deriving (Typeable) -- phantom types! fun!
type IScalarProxy = (Eval VScalar, (VScalar -> Eval ()))
type IScalarLazy = Maybe VScalar

-- these implementation allows no destructions
type IRule   = VRule
type IHandle = VHandle -- XXX maybe IORef?

instance Show IArray  where show _ = "{array}"
instance Show IHash   where show _ = "{hash}"
instance Show IScalar where show _ = "{scalar}"
-- instance Show IHandle where show _ = "{handle}"
-- instance Show ICode   where show _ = "{code}"
-- instance Show IRule   where show _ = "{rule}"

-- GADTs, here we come!
data VRef where
    MkRef   :: (Typeable a) => IVar a -> VRef

instance Typeable VRef where
    typeOf (MkRef x) = typeOf x

instance Typeable1 (ContT Val (ReaderT Env IO)) where
    typeOf1 _ = typeOf ' '

instance Typeable1 IVar where
    typeOf1 (IScalar x) = typeOf x
    typeOf1 (IArray  x) = typeOf x
    typeOf1 (IHash   x) = typeOf x
    typeOf1 (ICode   x) = typeOf x
    typeOf1 (IHandle x) = typeOf x
    typeOf1 (IRule   x) = typeOf x

instance Typeable2 HTable.HashTable where
    typeOf2 _ = typeOf ' '
