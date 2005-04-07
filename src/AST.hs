{-# OPTIONS_GHC -fglasgow-exts -fno-warn-orphans #-}

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
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.IntMap as Array

type Ident = String

ifContextIsa c trueM falseM = do
    env <- ask
    if isaType (envClasses env) c (envContext env)
        then trueM
        else falseM

fromVal' (VThunk (MkThunk eval)) = fromVal' =<< eval
fromVal' (MVal mval) = fromVal' =<< liftIO (readIORef mval)
fromVal' v = do
    rv <- liftIO $ catchJust errorCalls (return . Right $ vCast v) $
        \str -> return (Left str)
    case rv of
        Right v -> return v
        Left e  -> retError e (Val v) -- XXX: not working yet

fromMVal = (>>= fromVal) . readMVal

class Value n where
    fromVal :: Val -> Eval n
    fromVal = fromVal'
    vCast :: Val -> n
    -- vCast (MVal v)      = vCast $ castV v
    vCast (VRef v)      = vCast v
    vCast (VPair (_, v))   = vCast v
    vCast (VArray v)    = vCast $ VList $ Array.elems v
    vCast v             = doCast v
    castV :: n -> Val
    castV _ = error $ "cannot cast into Val"
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
        _       -> error $ "cannot cast into VPair: " ++ (show v)

instance Value VHash where
    vCast (VHash h) = h
    -- vCast VUndef = emptyFM
    vCast (VPair (k, v)) = Map.fromList [(vCast k, v)]
    vCast x = Map.fromList [ (vCast k, v) | (k, v) <- vCast x ]

instance Value [VPair] where
    -- vCast VUndef = []
    vCast (VRef v)      = vCast v
    vCast (VHash h) = [ (VStr k, v) | (k, v) <- Map.assocs h ]
    vCast (VPair p) = [p]
    vCast (VArray vs) = [ (castV k, v) | (k, v) <- Array.assocs vs ]
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
juncToBool (Junc JAny  _  vs) = (True `Set.member`) $ Set.map vCast vs
juncToBool (Junc JAll  _  vs) = not . (False `Set.member`) $ Set.map vCast vs
juncToBool (Junc JNone _  vs) = not . (True `Set.member`) $ Set.map vCast vs
juncToBool (Junc JOne  ds vs)
    | (True `Set.member`) $ Set.map vCast ds
    = False
    | otherwise
    = (1 ==) . length . filter vCast $ Set.elems vs

readMVal :: MonadIO m => Val -> m Val
readMVal (MVal mv) = readMVal =<< liftIO (readIORef mv)
readMVal v         = return v

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
    doCast (VArray a)   = toRational $ Array.size a
    doCast (VHash h)    = fromIntegral $ Map.size h
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
    doCast (VArray a)   = fromIntegral $ Array.size a
    doCast (VHash h)    = fromIntegral $ Map.size h
    doCast t@(VThread _)  = read $ vCast t
    doCast _            = 0/0 -- error $ "cannot cast as Num: " ++ (show x)

instance Value VComplex where
    castV = VComplex
    doCast x            = (vCast x :: VNum) :+ 0

instance Value VStr where
    castV = VStr
    fromVal (VHash h) = do
        ls <- mapM strPair $ Map.assocs h
        return $ unlines ls
        where
        strPair (k, v) = do
            -- k' <- fromMVal k
            v' <- fromMVal v
            return $ k ++ "\t" ++ v'
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
    vCast (VRef v)      = vCast v
    -- vCast (MVal v)      = vCast $ castV v
    vCast (VPair (k, v))= vCast k ++ "\t" ++ vCast v ++ "\n"
    vCast (VArray l)    = unwords . map vCast $ Array.elems l
    vCast (VHash h)     = unlines $
        map (\(k, v) -> (k ++ "\t" ++ vCast v)) $ Map.assocs h
    vCast (VSub s)      = "<" ++ show (subType s) ++ "(" ++ subName s ++ ")>"
    vCast (VJunc j)     = show j
    vCast (VThread t)   = dropWhile (not . isDigit) $ show t
    vCast x             = error $ "cannot cast as Str: " ++ (show x)

showNum :: Show a => a -> String
showNum x
    | (i, ".0") <- break (== '.') str
    = i -- strip the trailing ".0"
    | otherwise = str
    where
    str = show x 

instance Value VArray where
    castV = VArray
    vCast x = Array.fromAscList $ [1..] `zip` vCast x

instance Value MVal where
    castV _ = error "Cannot cast MVal into Value"
    fromVal (MVal x) = return x
    fromVal (VRef v) = fromVal v
    fromVal (VPair (_, v)) = fromVal v
    fromVal v = retError "cannot modify constant item" $ Val v
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
    vCast (VArray l)    = Array.elems l
    vCast (VHash h)     = [ VPair (VStr k, v) | (k, v) <- Map.assocs h ]
    vCast (VPair (k, v))   = [k, v]
    vCast (VRef v)      = vCast v
    -- vCast (MVal v)      = vCast $ castV v
    vCast (VUndef)      = [VUndef]
    vCast v             = [v]

instance Value VHandle where
    castV = VHandle
    doCast (VHandle x) = x
    doCast x            = error $ "cannot cast into a handle: " ++ show x

instance Value VSocket where
    castV = VSocket
    doCast (VSocket x) = x
    doCast x            = error $ "cannot cast into a socket: " ++ show x

instance Value VThread where
    castV = VThread
    doCast (VThread x) = x
    doCast x            = error $ "cannot cast into a thread: " ++ show x

instance Value (Maybe a) where
    vCast VUndef        = Nothing
    vCast _             = Just undefined

instance Value Int   where
    doCast = intCast
    castV = VInt . fromIntegral
instance Value Word  where doCast = intCast
instance Value Word8 where doCast = intCast
instance Value [Word8] where doCast = map (toEnum . ord) . vCast

type VScalar = Val
-- type VJunc = Set Val

instance Value VScalar where
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

type VBool = Bool
type VInt  = Integer
type VRat  = Rational
type VNum  = Double
type VComplex = Complex VNum
type VStr  = String
type VList = [Val]
type VSubst = (VRule, Exp)
type VHandle = Handle
type VSocket = Socket
type VThread = ThreadId
type MVal = IORef Val
type VArray = Array.IntMap Val
type VHash = Map VStr Val -- deriving (Show, Eq, Ord)
newtype VThunk = MkThunk (Eval Val)
data VRule     = MkRule
    { rxRegex     :: Regex
    , rxGlobal    :: Bool
    }
    deriving (Show, Eq, Ord)

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
    | VArray    VArray
    | VHash     VHash
    | VRef      Val
    | VPair     VPair
    | VSub      VSub
    | VBlock    VBlock
    | VJunc     VJunc
    | VError    VStr Exp
    | VHandle   VHandle
    | VSocket   VSocket
    | VThread   VThread
    | VRule     VRule
    | VSubst    VSubst
    | MVal      MVal
    | VControl  VControl
    | VThunk    VThunk
    deriving (Show, Eq, Ord)

valType :: Val -> String
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
valType (VSocket  _)    = "Socket"
valType (VThread  _)    = "Thread"
valType (MVal     _)    = "Var"
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

data VSub = Sub
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
    deriving (Show, Eq, Ord)

instance Ord VComplex where {- ... -}
instance Ord MVal where
    compare _ _ = EQ -- compare (castV x) (castV y)
instance Show MVal where
    show _ = "<mval>"
instance Show (IORef Pad) where
    show _ = "<pad>"
instance Ord VHandle where
    compare x y = compare (show x) (show y)
instance Ord VSocket where
    compare x y = compare (show x) (show y)

type Var = String
-- type MVal = IORef Val

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

cxtOfSigil :: Char -> String
cxtOfSigil '$'  = "Scalar"
cxtOfSigil '@'  = "Array"
cxtOfSigil '%'  = "Hash"
cxtOfSigil '&'  = "Code"
cxtOfSigil x    = internalError $ "cxtOfSigil: unexpected character: " ++ (show x)

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

type Pad = [Symbol Val]
data Symbol a where
    SymVal :: Scope -> String -> Val -> Symbol Val
    SymExp :: Scope -> String -> Exp -> Symbol Exp

show' :: (Show a) => a -> String
show' x = "( " ++ show x ++ " )"

instance Show (Symbol a) where
    show (SymVal s n v) = unwords [ "SymVal", show' s, show' n, show' v ]
    show (SymExp s n e) = unwords [ "SymExp", show' s, show' n, show' e ]

instance Eq (Symbol a) where
    x == y = (show x) == (show y)

instance Ord (Symbol a) where
    compare x y = compare (show x) (show y)

symScope (SymVal s _ _) = s
symScope (SymExp s _ _) = s
symName (SymVal _ n _) = n
symName (SymExp _ n _) = n
symVal (SymVal _ _ v) = v
symVal _ = error "Cannot cast SymVal to SymExp"
symExp (SymExp _ _ e) = e
symExp _ = error "Cannot cast SymExp to SymVal"

data Scope = SGlobal | SMy | SOur | SLet | STemp | SState
    deriving (Show, Eq, Ord, Read, Enum)

type Eval x = ContT Val (ReaderT Env IO) x

findSym :: String -> Pad -> Maybe Val
findSym name pad = do
    s <- find ((== name) . symName) pad
    return $ symVal s

writeMVal l (MVal r)     = writeMVal l =<< liftIO (readIORef r)
writeMVal (MVal l) r     = liftIO $ writeIORef l r
writeMVal (VThunk (MkThunk t)) r = do
    l <- t
    writeMVal l r
writeMVal (VError s e) _ = retError s e
writeMVal _ (VError s e) = retError s e
writeMVal (VControl c) _ = retControl c
writeMVal _ (VControl c) = retControl c
writeMVal x _            = retError "Can't write a constant item" (Val x)

askGlobal :: Eval Pad
askGlobal = do
    glob <- asks envGlobal
    liftIO $ readIORef glob

readVar name = do
    glob <- askGlobal
    case find ((== name) . symName) glob of
        Just ref -> readMVal $ symVal ref
        _ -> return VUndef

emptyExp = App "&not" [] []

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

