{-# OPTIONS_GHC -fglasgow-exts -fno-warn-orphans -funbox-strict-fields #-}
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

module Pugs.AST where
import Pugs.Internals
import Pugs.Context
import Pugs.Rule
import List
import Pugs.Types
import qualified Pugs.Types.Array  as Array
import qualified Pugs.Types.Handle as Handle
import qualified Pugs.Types.Hash   as Hash
import qualified Pugs.Types.Scalar as Scalar
import qualified Pugs.Types.Code   as Code
import qualified Pugs.Types.Thunk  as Thunk
import qualified Pugs.Types.Rule   as Rule
import qualified Pugs.Types.Pair   as Pair
import qualified Pugs.Types.Object as Object
import qualified Data.Set       as Set
import qualified Data.Map       as Map
import qualified Data.HashTable as HTable

type Ident = String

errIndex (Just v) _ = return v
errIndex _ idx =
    retError "Modification of non-creatable array value attempted"
        (Val $ castV idx)

-- Three outcomes: Has value; can extend; cannot extend
getIndex :: Int -> Maybe a -> Eval [a] -> Maybe (Eval b) -> Eval a

getIndex idx def doList _ | idx < 0 = do
    -- first, check if the list is at least abs(idx) long.
    list <- doList
    if null (drop (abs (idx+1)) list)
        then errIndex def idx
        else return (list !! (idx `mod` (length list)))

-- now we are all positive; either extend or return
getIndex idx def doList ext = do
    list <- doList
    case drop idx list of
        [] -> case ext of
            Just doExt -> do { doExt ; getIndex idx def doList Nothing }
            Nothing    -> errIndex def idx
        (a:_) -> return a

ifValTypeIsa :: Val -> String -> (Eval a) -> (Eval a) -> Eval a
ifValTypeIsa v typ trueM falseM = do
    env <- ask
    vt  <- evalValType v
    if isaType (envClasses env) typ vt
        then trueM
        else falseM

ifListContext :: (MonadReader Env m) => m t -> m t -> m t
ifListContext trueM falseM = do
    cxt <- asks envContext
    case cxt of
        CxtSlurpy _   -> trueM
        _           -> falseM

retEmpty :: ContT Val (ReaderT Env IO) Val
retEmpty = do
    ifListContext
        (return $ VList [])
        (return VUndef)

{-
ifContextIsa :: (MonadReader Env m) => Cxt -> m t -> m t -> m t
ifContextIsa c trueM falseM = do
    Env{ envClasses = cls, envContext = cxt } <- ask
    if isaType cls c cxt
        then trueM
        else falseM
-}

evalValType :: Val -> Eval Type
evalValType (VRef r) = do
    cls <- asks envClasses
    let typ = refType r
    if isaType cls "Scalar" typ
        then evalValType =<< readRef r
        else return typ
evalValType val = return $ valType val

fromVal' :: (Value a) => Val -> Eval a
fromVal' (VRef r) = do
    v <- readRef r
    fromVal v
fromVal' (VList vs) | not $ null [ undefined | VRef _ <- vs ] = do
    vs <- forM vs $ \v -> case v of { VRef r -> readRef r; _ -> return v }
    fromVal $ VList vs
fromVal' v = do
    rv <- liftIO $ catchJust errorCalls (return . Right $ vCast v) $
        \str -> return (Left str)
    case rv of
        Right v -> return v
        Left e  -> retError e (Val v) -- XXX: not working yet

class (Typeable n) => Value n where
    fromVal :: Val -> Eval n
    fromVal = fromVal'
    vCast :: Val -> n
    vCast v@(VRef _)    = castFail v
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
    fromVal v = return (vCast v)
    vCast (VRef r)   = r
    vCast (VList vs) = arrayRef vs
    vCast v          = scalarRef v
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
    castV pv = VRef $ pairRef pv
    fromVal VUndef  = return (VUndef, VUndef)
    fromVal v = do
        vs <- fromVal' v
        case vs of
            [x, y]  -> return (x, y)
            _       -> castFail v

instance Value VHash where
    fromVal v = do
        list <- fromVal v
        forM list $ \(k, v) -> do
            str <- fromVal k
            return (str, v)

instance Value [VPair] where
    fromVal v = do
        list <- fromVals v
        doFrom $ concat list
        where
        doFrom [] = return []
        doFrom (k:v:list) = do
            rest <- doFrom list
            return ((k, v):rest)
        doFrom [k] = do
            -- XXX: warn about odd elements?
            return [(k, undef)]

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
    doCast t@(VThread _)  = read $ vCast t
    doCast _            = 0/0 -- error $ "cannot cast as Num: " ++ show x

instance Value VComplex where
    castV = VComplex
    doCast x            = (vCast x :: VNum) :+ 0

instance Value VStr where
    castV = VStr
    fromVal (VList l)   = return . unwords =<< mapM fromVal l
    fromVal v = do
        vt  <- evalValType v
        if vt /= mkType "Hash" then fromVal' v else do
        --- XXX special case for Hash -- need to Objectify
        hv      <- join $ doHash v Hash.fetch
        lns     <- forM hv $ \(k, v) -> do
            str <- fromVal v
            return $ k ++ "\t" ++ str
        return $ unlines lns
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
    vCast (VCode s)      = "<" ++ show (subType s) ++ "(" ++ subName s ++ ")>"
    vCast (VJunc j)     = show j
    vCast (VThread t)   = dropWhile (not . isDigit) $ show t
    vCast x             = castFail x

showNum :: Show a => a -> String
showNum x
    | str == "Infinity"
    = "Inf"
    | (i, ".0") <- break (== '.') str
    = i -- strip the trailing ".0"
    | otherwise = str
    where
    str = show x 

valToStr :: Val -> Eval VStr
valToStr = fromVal

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

instance Value VProcess where
    castV = VProcess
    doCast (VProcess x)  = x
    doCast x            = castFail x

instance Value Int   where
    doCast = intCast
    castV = VInt . fromIntegral
instance Value Word  where doCast = intCast
instance Value Word8 where doCast = intCast
instance Value [Word8] where doCast = map (toEnum . ord) . vCast

type VScalar = Val

instance Value VScalar where
    fromVal (VRef r) = fromVal =<< readRef r
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
    deriving (Typeable)
newtype VProcess = MkProcess (ProcessHandle)
    deriving (Typeable)

type VPair = (Val, Val)

data Val
    = VUndef
    | VBool     !VBool
    | VInt      !VInt
    | VRat      !VRat
    | VNum      !VNum
    | VComplex  !VComplex
    | VStr      !VStr
    | VList     VList -- Lists are lazy.
    | VRef      !VRef
    | VCode     !VCode
    | VBlock    !VBlock
    | VJunc     !VJunc
    | VError    !VStr !Exp
    | VHandle   !VHandle
    | VSocket   !VSocket
    | VThread   !VThread
    | VProcess  !VProcess
    | VRule     !VRule
    | VSubst    !VSubst
    | VControl  !VControl
    deriving (Show, Eq, Ord, Typeable)

valType :: Val -> Type
valType VUndef          = mkType "Scalar"
valType (VRef v)        = refType v
valType (VBool    _)    = mkType "Bool"
valType (VInt     _)    = mkType "Int"
valType (VRat     _)    = mkType "Rat"
valType (VNum     _)    = mkType "Num"
valType (VComplex _)    = mkType "Complex"
valType (VStr     _)    = mkType "Str"
valType (VList    _)    = mkType "List"
valType (VCode    c)    = Code.iType c
valType (VBlock   _)    = mkType "Block"
valType (VJunc    _)    = mkType "Junction"
valType (VError _ _)    = mkType "Error"
valType (VHandle  _)    = mkType "IO"
valType (VSocket  _)    = mkType "Socket"
valType (VThread  _)    = mkType "Thread"
valType (VProcess _)    = mkType "Process"
valType (VControl _)    = mkType "Control"
valType (VRule    _)    = mkType "Rule"
valType (VSubst   _)    = mkType "Subst"

type VBlock = Exp
data VControl
    = ControlLeave !(Env -> Eval Bool) !Val
    | ControlExit  !ExitCode
    | ControlEnv   !Env
    deriving (Show, Eq, Ord)

data VJunc = Junc { juncType :: !JuncType
                  , juncDup  :: !(Set Val)
                  , juncSet  :: !(Set Val)
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

isSlurpy :: Param -> Bool
isSlurpy param = isSlurpyCxt $ paramContext param

data Param = MkParam
    { isInvocant    :: !Bool
    , isOptional    :: !Bool
    , isNamed       :: !Bool
    , isLValue      :: !Bool
    , isWritable    :: !Bool
    , isThunk       :: !Bool
    , paramName     :: !String
    , paramContext  :: !Cxt
    , paramDefault  :: !Exp
    }
    deriving (Show, Eq, Ord)

type Params     = [Param]
type Bindings   = [(Param, Exp)]
type SlurpLimit = [(VInt, Exp)]

data VCode = MkCode
    { isMulti       :: !Bool
    , subName       :: !String
    , subType       :: !SubType
    , subPad        :: !Pad
    , subAssoc      :: !String
    , subParams     :: !Params
    , subBindings   :: !Bindings
    , subSlurpLimit :: !SlurpLimit
    , subReturns    :: !Type
    , subFun        :: !Exp
    }
    deriving (Show, Eq, Ord, Typeable)

mkPrim = MkCode
    { isMulti = True
    , subName = "&?"
    , subType = SubPrim
    , subPad = mkPad []
    , subAssoc = "pre"
    , subParams = []
    , subBindings = []
    , subSlurpLimit = []
    , subReturns = anyType
    , subFun = emptyExp
    }

mkSub = MkCode
    { isMulti = False
    , subName = "&?"
    , subType = SubBlock
    , subPad = mkPad []
    , subAssoc = "pre"
    , subParams = []
    , subBindings = []
    , subSlurpLimit = []
    , subReturns = anyType
    , subFun = emptyExp
    }

instance Ord VComplex where {- ... -}
instance (Typeable a) => Show (IORef a) where
    show _ = "<ref>"

data Exp
    = Noop
    | App !String ![Exp] ![Exp]
    | Syn !String ![Exp]
    | Cxt !Cxt !Exp
    | Sym !Scope !Var
    | Pad !Scope !Pad
    | Prim !([Val] -> Eval Val)
    | Val !Val
    | Var !Var
    | Parens !Exp
    | NonTerm !SourcePos
    | Stmts ![(Exp, SourcePos)]
    deriving (Show, Eq, Ord)

fromVals :: (Value n) => Val -> Eval [n]
fromVals v = mapM fromVal =<< fromVal v

instance Show VThunk where
    show _ = "<thunk>"
instance Eq VThunk
instance Ord VThunk where
    compare _ _ = EQ

instance Show VProcess where
    show _ = "<process>"
instance Eq VProcess
instance Ord VProcess where
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
extract ((Stmts stmts), vs) = (Stmts stmts', vs')
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
extract ((Cxt cxt ex), vs) = ((Cxt cxt ex'), vs')
    where
    (ex', vs') = extract (ex, vs)
extract ((Parens ex), vs) = ((Parens ex'), vs')
    where
    (ex', vs') = extract (ex, vs)
extract other = other

cxtOfExp :: Exp -> Eval Cxt
cxtOfExp (Cxt cxt _)            = return cxt
cxtOfExp (Syn "," _)            = return cxtSlurpyAny
cxtOfExp (Syn "[]" [_, exp])    = cxtOfExp exp
cxtOfExp (Syn "{}" [_, exp])    = cxtOfExp exp
cxtOfExp (Val (VList _))        = return cxtSlurpyAny
cxtOfExp (Val (VRef ref))       = do
    cls <- asks envClasses
    let typ = refType ref
    return $ if isaType cls "List" typ
        then cxtSlurpyAny
        else CxtItem typ
cxtOfExp (Val _)                = return cxtItemAny
cxtOfExp (Var (c:_))            = return $ cxtOfSigil c
cxtOfExp (App "&list" _ _)      = return cxtSlurpyAny
cxtOfExp _                      = return cxtItemAny

cxtOfSigil :: Char -> Cxt
cxtOfSigil '$'  = cxtItemAny
cxtOfSigil '@'  = cxtSlurpyAny
cxtOfSigil '%'  = cxtSlurpyAny
cxtOfSigil '&'  = CxtItem $ mkType "Code"
cxtOfSigil x    = internalError $ "cxtOfSigil: unexpected character: " ++ show x

typeOfSigil :: Char -> Type
typeOfSigil '$'  = mkType "Scalar"
typeOfSigil '@'  = mkType "Array"
typeOfSigil '%'  = mkType "Hash"
typeOfSigil '&'  = mkType "Code"
typeOfSigil x    = internalError $ "typeOfSigil: unexpected character: " ++ show x

buildParam :: String -> String -> String -> Exp -> Param
buildParam typ sigil name e = MkParam
    { isInvocant    = False
    , isOptional    = (sigil ==) `any` ["?", "+"]
    , isNamed       = (null sigil || head sigil /= '+')
    , isLValue      = True
    , isWritable    = False
    , isThunk       = False
    , paramName     = name
    , paramContext  = case sigil of
        ('*':_) -> CxtSlurpy typ'
        _       -> CxtItem typ'
    , paramDefault  = e
    }
    where
    typ' = if null typ then typeOfSigil (head name) else mkType typ

defaultArrayParam :: Param
defaultHashParam :: Param
defaultScalarParam :: Param

defaultArrayParam   = buildParam "" "*" "@_" (Val VUndef)
defaultHashParam    = buildParam "" "*" "%_" (Val VUndef)
defaultScalarParam  = buildParam "" "*" "$_" (Val VUndef)

data Env = Env { envContext :: !Cxt
               , envLValue  :: !Bool
               , envLexical :: !Pad
               , envGlobal  :: !(IORef Pad)
               , envClasses :: !ClassTree
               , envEval    :: !(Exp -> Eval Val)
               , envCaller  :: !(Maybe Env)
               , envBody    :: !Exp
               , envDepth   :: !Int
               , envID      :: !Unique
               , envDebug   :: !(Maybe (IORef (Map String String)))
               , envStash   :: !String
               } deriving (Show, Eq, Ord)

envWant env = 
    showCxt (envContext env) ++ (if envLValue env then ", LValue" else "")
    where
    showCxt CxtVoid         = "Void"
    showCxt (CxtItem typ)   = "Scalar (" ++ showType typ ++ ")"
    showCxt (CxtSlurpy typ) = "List (" ++ showType typ ++ ")"

newtype Pad = MkPad (Map Var [IORef VRef])
    deriving (Eq, Ord, Typeable)

instance Show Pad where
    show pad = "(mkPad [" ++ 
                concat (intersperse ", " $ map dump $ padToList pad) ++
                "])"
        where
        dump (n, ioRefs) = "(" ++ show n ++ ", [" ++
                            concat (intersperse ", " $ map dumpIORef ioRefs) ++
                            "])"
        dumpIORef ioRef = unsafePerformIO $ do
            ref  <- readIORef ioRef
            dump <- (`runReaderT` undefined) $ (`runContT` return) $ resetT $ do
                dumpRef ref
            return $ "unsafePerformIO (newIORef " ++ vCast dump ++ ")"

mkPad = MkPad . Map.fromList
lookupPad key (MkPad map) = Map.lookup key map
padToList (MkPad map) = Map.assocs map
diffPads (MkPad map1) (MkPad map2) = MkPad $ Map.difference map1 map2
unionPads (MkPad map1) (MkPad map2) = MkPad $ Map.union map1 map2

genMultiSym name ref = liftIO $ do
    ioRef <- newIORef ref
    return $ \(MkPad map) -> MkPad $ Map.insertWith (flip (++)) name [ioRef] map

genSym name ref = liftIO $ do
    ioRef <- newIORef ref
    return $ \(MkPad map) -> MkPad $ Map.insert name [ioRef] map

show' :: (Show a) => a -> String
show' x = "( " ++ show x ++ " )"

data Scope = SGlobal | SMy | SOur | SLet | STemp | SState
    deriving (Show, Eq, Ord, Read, Enum)

type Eval x = ContT Val (ReaderT Env IO) x

runEval :: Env -> Eval Val -> IO Val
runEval env eval = withSocketsDo $ do
    my_perl <- initPerl5 ""
    val <- (`runReaderT` env) $ do
        (`runContT` return) $
            resetT eval
    freePerl5 my_perl
    return val

findSymRef :: (MonadIO m) => String -> Pad -> m VRef
findSymRef name pad = do
    case findSym name pad of
        Just ref -> liftIO $ readIORef ref
        Nothing  -> fail $ "oops, can't find " ++ name

findSym :: String -> Pad -> Maybe (IORef VRef)
findSym name pad = case lookupPad name pad of
    Just (x:_)  -> Just x
    _           -> Nothing

{-
cloneEnv env@Env{ envLexical = lex, envGlobal = globRef } = liftIO $ do
    glob     <- readIORef globRef
    lex'     <- sequence $ fmap readIORef lex
    glob'    <- sequence $ fmap readIORef glob
    globRef' <- newIORef glob'
    return $ env{ envLexical = lex', envGlobal = globRef' }

symRef sym = liftIO . readIORef $ symVar sym

cloneSym ref = readIORef
    rea
    genSym (symName sym) =<< symRef sym
-}


askGlobal :: Eval Pad
askGlobal = do
    glob <- asks envGlobal
    liftIO $ readIORef glob

writeVar :: Var -> Val -> Eval ()
writeVar name val = do
    glob <- askGlobal
    case findSym name glob of
        Just ioRef -> do
            ref <- liftIO $ readIORef ioRef
            writeRef ref val
        _        -> return () -- XXX Wrong

readVar :: Var -> Eval Val
readVar name = do
    glob <- askGlobal
    case findSym name glob of
        Just ioRef -> do
            ref <- liftIO $ readIORef ioRef
            readRef ref
        _        -> return undef

emptyExp = Noop

retControl :: VControl -> Eval a
retControl c = do
    shiftT $ const (return $ VControl c)

retError :: VStr -> Exp -> Eval a
retError str (Val VUndef) = retError str (Val $ VStr str)
retError str _ = do
    -- get stuff
    glob <- askGlobal
    file <- fromVal =<< readRef =<< findSymRef "$?FILE" glob
    line <- fromVal =<< readRef =<< findSymRef "$?LINE" glob
    col  <- fromVal =<< readRef =<< findSymRef "$?COLUMN" glob
    shiftT $ const (return $ VError str (NonTerm $ SourcePos file line col))

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
            notFollowedBy . satisfy $ \x -> 
                (isAlpha x && ((x /=) `all` "eE"))
                || ((x ==) `any` ".=")
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

forceRef :: VRef -> Eval Val
forceRef (MkRef (IScalar sv)) = forceRef =<< fromVal =<< Scalar.fetch sv
forceRef (MkRef (IThunk tv)) = Thunk.force tv
forceRef r = retError "cannot forceRef" (Val $ VRef r)

dumpRef :: VRef -> Eval Val
dumpRef (MkRef (ICode cv)) = do
    vsub <- Code.assuming cv [] []
    return (VStr $ "(MkRef (ICode $ " ++ show vsub ++ "))")
dumpRef (MkRef (IScalar sv)) | Scalar.iType sv == mkType "Scalar::Const" = do
    sv <- Scalar.fetch sv
    return (VStr $ "(MkRef (IScalar $ " ++ show sv ++ "))")
dumpRef ref = return (VStr $ "(unsafePerformIO . newObject $ mkType \"" ++ show (refType ref) ++ "\")")

readRef :: VRef -> Eval Val
readRef (MkRef (IScalar sv)) = Scalar.fetch sv
readRef (MkRef (ICode cv)) = do
    vsub <- Code.assuming cv [] []
    return $ VCode vsub
readRef (MkRef (IHash hv)) = do
    pairs <- Hash.fetch hv
    return $ VList $ map (\(k, v) -> castV (castV k, v)) pairs
readRef (MkRef (IArray av)) = do
    vals <- Array.fetch av
    return $ VList vals
readRef (MkRef (IPair pv)) = do
    (k, v) <- Pair.fetch pv
    return $ VList [k, v]
readRef (MkRef (IHandle io)) = return . VHandle =<< Handle.fetch io
readRef (MkRef (IRule rx)) = return . VRule =<< Rule.fetch rx
readRef (MkRef (IThunk tv)) = readRef =<< fromVal =<< Thunk.force tv

retIVar :: (Typeable a) => IVar a -> Eval Val 
retIVar = return . VRef . MkRef

writeRef :: VRef -> Val -> Eval ()
writeRef (MkRef (IScalar s)) (VList vals) = do
    av <- liftIO (newArray vals)
    Scalar.store s (VRef $ MkRef av)
writeRef (MkRef (IScalar s)) val = Scalar.store s val
writeRef (MkRef (IArray s)) val  = Array.store s =<< fromVals val
writeRef (MkRef (IHash s)) val   = Hash.store s =<< fromVal val
writeRef (MkRef (ICode s)) val   = Code.store s =<< fromVal val
writeRef (MkRef (IPair s)) val   = Pair.storeVal s val
writeRef (MkRef (IThunk tv)) val = (`writeRef` val) =<< fromVal =<< Thunk.force tv
writeRef r _ = retError "cannot writeRef" (Val $ VRef r)

clearRef :: VRef -> Eval ()
clearRef (MkRef (IScalar s)) = Scalar.store s undef
clearRef (MkRef (IArray s))  = Array.clear s
clearRef (MkRef (IHash s))   = Hash.clear s
clearRef (MkRef (IPair s))   = Pair.storeVal s undef
clearRef (MkRef (IThunk tv)) = clearRef =<< fromVal =<< Thunk.force tv
clearRef r = retError "cannot clearRef" (Val $ VRef r)

newObject :: (MonadIO m) => Type -> m VRef
newObject (MkType "Scalar") = liftIO $
    return . scalarRef =<< newIORef undef
newObject (MkType "Array")  = liftIO $
    return . arrayRef =<< (newIORef [] :: IO IArray)
newObject (MkType "Hash")   = liftIO $
    return . hashRef =<< (HTable.new (==) HTable.hashString :: IO IHash)
newObject (MkType "Code")   = liftIO $
    return . codeRef =<< newIORef mkSub
newObject typ = fail ("Cannot create object: " ++ showType typ)

-- XXX: Refactor doHash and doArray into one -- also see Eval's [] and {}
doHash :: Val -> (forall a. Hash.Class a => a -> b) -> Eval b
doHash (VRef (MkRef (IHash hv))) f = return $ f hv
doHash (VRef (MkRef (IScalar sv))) f = do
    val <- Scalar.fetch sv
    case val of
        VUndef  -> do
            ref@(MkRef (IHash hv)) <- newObject (MkType "Hash")
            Scalar.store sv (VRef ref)
            return $ f hv
        _  -> doHash val f
doHash (VRef (MkRef p@(IPair _))) f = return $ f p
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
            ref@(MkRef (IArray hv)) <- newObject (MkType "Array")
            Scalar.store sv (VRef ref)
            return $ f hv
doArray (VRef (MkRef p@(IPair _))) f = return $ f p
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
    IThunk  :: Thunk.Class  a => a -> IVar VThunk
    IPair   :: Pair.Class   a => a -> IVar VPair

readIVar :: IVar v -> Eval v
readIVar (IScalar x) = Scalar.fetch x
readIVar (IPair x)   = Pair.fetch x
readIVar (IArray x)  = Array.fetch x
readIVar (IHash x)   = Hash.fetch x
readIVar _ = error "readIVar"

writeIVar :: IVar v -> v -> Eval ()
writeIVar (IScalar x) = Scalar.store x
writeIVar _ = error "writeIVar"

refType (MkRef x) = Object.iType x

instance (Typeable a) => Object.Class (IVar a) where
    iType (IScalar x) = Scalar.iType x
    iType (IArray x)  = Array.iType x
    iType (IHash x)   = Hash.iType x
    iType (ICode x)   = Code.iType x
    iType (IHandle x) = Handle.iType x
    iType (IRule x)   = Rule.iType x
    iType (IThunk x)  = Thunk.iType x
    iType (IPair x)   = Pair.iType x

instance Eq VRef where
    (==) = const $ const False
instance Ord VRef where
    compare _ _ = EQ
instance Show VRef where
    show v = "<" ++ showType (refType v) ++ ">"

instance Eq (IVar a) where
    (==) = const $ const False
instance Ord (IVar a) where
    compare _ _ = EQ
instance Ord (IORef a) where
    compare _ _ = EQ
instance (Typeable a) => Show (IVar a) where
    show v = show (MkRef v)

scalarRef x = MkRef (IScalar x)
codeRef x   = MkRef (ICode x)
arrayRef x  = MkRef (IArray x)
hashRef x   = MkRef (IHash x)
thunkRef x  = MkRef (IThunk x)
pairRef x   = MkRef (IPair x)

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

instance Pair.Class VPair where
    fetchKey = return . fst
    fetchVal = return . snd
    storeVal pv val = do
        ref <- fromVal (snd pv)
        writeRef ref val

instance Scalar.Class IScalarProxy where
    iType = const $ mkType "Scalar::Proxy"
    fetch = fst
    store = snd

instance Array.Class (IVar VPair) where
    iType = const $ mkType "Pair"
    fetch pv = do
        (k, v)  <- readIVar pv
        return [k, v]
    existsElem _ idx = return (idx >= -2 || idx <= 1)
    fetchSize        = const $ return 2
    fetchVal pv (-2) = return . fst =<< readIVar pv
    fetchVal pv (-1) = return . snd =<< readIVar pv
    fetchVal pv 0    = return . fst =<< readIVar pv
    fetchVal pv 1    = return . snd =<< readIVar pv
    fetchVal _  _    = return undef
    storeVal _ _ _   = retConstError undef
    storeElem _ _ _  = retConstError undef
    deleteElem _ _   = retConstError undef

instance Hash.Class (IVar VPair) where
    iType = const $ mkType "Pair"
    fetch pv = do
        (k, v)  <- readIVar pv
        str     <- fromVal k
        return [(str, v)]
    fetchVal pv idx = do
        (k, v)  <- readIVar pv
        str     <- fromVal k
        if str == idx
            then return v
            else return undef
    storeVal _ _ _ = retConstError undef
    deleteElem _ _ = retConstError undef

instance Hash.Class VHash where
    iType = const $ mkType "Hash::Const"
    fetch = return
    fetchKeys = return . map fst
    fetchVal hv idx = return . maybe undef id $ lookup idx hv
    clear _ = retConstError undef
    store _ _ = retConstError undef
    storeVal _ _ _ = retConstError undef
    storeElem _ _ _ = retConstError undef
    deleteElem _ _ = retConstError undef

instance Array.Class VArray where
    iType = const $ mkType "Array::Const"
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
    fetchVal av idx = getIndex idx (Just undef) (return av) Nothing
    storeVal _ _ _ = retConstError undef
    storeElem _ _ _ = retConstError undef

instance Hash.Class IHashEnv where
    iType = const $ mkType "Hash::Env"
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
    iType = const $ mkType "Array::Slice"
    store av vals = mapM_ (uncurry writeIVar) (zip av vals)
    fetchSize = return . length
    fetchElem av idx = getIndex idx Nothing (return av) Nothing
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
        readIVar =<< getIndex idx (Just $ constScalar undef)
            (liftIO $ readIORef av) 
            Nothing -- don't bother extending
    fetchKeys av = do
        svList <- liftIO $ readIORef av
        return $ zipWith const [0..] svList
    fetchElem av idx = do
        sv <- getIndex idx Nothing
            (liftIO $ readIORef av) 
            (Just (Array.extendSize av $ idx+1))
        if refType (MkRef sv) == mkType "Scalar::Lazy"
            then do
                val <- readIVar sv
                sv' <- newScalar val
                liftIO . modifyIORef av $ \svList ->
                    let idx' = idx `mod` length svList in
                    take idx' svList ++ (sv' : drop (idx'+1) svList)
                return sv'
            else return sv
    existsElem av idx | idx < 0 = Array.existsElem av (abs idx - 1)
    existsElem av idx = do
        svList <- liftIO $ readIORef av
        return . not . null $ drop idx svList
    deleteElem av idx = do
        liftIO . modifyIORef av $ \svList ->
            let idx' | idx < 0   = idx `mod` length svList -- XXX wrong; wraparound
                     | otherwise = idx in
            if null $ drop (idx' + 1) svList
                then take idx' svList
                else take idx' svList ++ (lazyUndef : drop (idx'+1) svList)
    storeElem av idx sv = do
        liftIO . modifyIORef av $ \svList ->
            let idx' | idx < 0   = idx `mod` length svList -- XXX wrong; wraparound
                     | otherwise = idx in
            take idx svList ++ (sv : drop (idx'+1) svList)

instance Handle.Class IHandle where
    fetch = return
    store = error "store"

instance Scalar.Class IScalar where
    fetch = liftIO . readIORef
    store = (liftIO .) . writeIORef

instance Scalar.Class IScalarLazy where
    iType = const $ mkType "Scalar::Lazy"
    fetch = return . maybe undef id
    store _ v = retConstError v

instance Scalar.Class IScalarCwd where
    iType = const $ mkType "Scalar::Cwd"
    fetch _ = do
        str <- liftIO $ getCurrentDirectory
	return $ VStr str
    store _ val = do
        str <- fromVal val
	tryIO () $ setCurrentDirectory str

instance Scalar.Class VScalar where
    iType = const $ mkType "Scalar::Const"
    fetch (VRef ref) = readRef ref
    fetch v = return v
    store _ v = retConstError v

instance Code.Class ICode where
    iType c  = Code.iType . unsafePerformIO $ readIORef c
    fetch    = liftIO . readIORef
    store    = (liftIO .) . writeIORef
    assuming c [] [] = Code.fetch c
    assuming _ _ _   = undefined
    apply    = error "apply"
    assoc c  = Code.assoc . unsafePerformIO $ readIORef c
    params c = Code.params . unsafePerformIO $ readIORef c

instance Code.Class VCode where
    -- XXX - subType should really just be a mkType itself
    iType c  = case subType c of
        SubBlock    -> mkType "Block"
        SubRoutine  -> mkType "Sub"
        SubPrim     -> mkType "Sub"
        SubMethod   -> mkType "Method"
    fetch    = return
    store _ _= retConstError undef
    assuming c [] [] = return c
    assuming _ _ _   = error "assuming"
    apply    = error "apply"
    assoc    = subAssoc
    params   = subParams

instance Thunk.Class VThunk where
    force (MkThunk c) = c

retConstError v = retError "Can't modify constant item" (Val v)

type IArray  = IORef [IVar VScalar]
type IArraySlice = [IVar VScalar]
type IHash   = HTable.HashTable VStr (IVar VScalar)
type IScalar = IORef Val
type ICode   = IORef VCode
data IHashEnv deriving (Typeable) -- phantom types! fun!
data IScalarCwd deriving (Typeable) -- phantom types! fun!
type IScalarProxy = (Eval VScalar, (VScalar -> Eval ()))
type IScalarLazy = Maybe VScalar

-- these implementation allows no destructions
type IRule   = VRule
type IHandle = VHandle -- XXX maybe IORef?

instance Show IHash   where show _ = "{hash}"
-- instance Show IArray  where show _ = "{array}"
-- instance Show IScalar where show _ = "{scalar}"
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
    typeOf1 (IThunk  x) = typeOf x
    typeOf1 (IPair   x) = typeOf x

instance Typeable2 HTable.HashTable where
    typeOf2 _ = typeOf ' '
