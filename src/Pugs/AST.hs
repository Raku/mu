{-# OPTIONS_GHC -cpp -fglasgow-exts -fno-warn-orphans -funbox-strict-fields #-}
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
import qualified Data.Set       as Set
import qualified Data.Map       as Map

#include "Types/Array.hs"
#include "Types/Handle.hs"
#include "Types/Hash.hs"
#include "Types/Scalar.hs"
#include "Types/Code.hs"
#include "Types/Thunk.hs"
#include "Types/Rule.hs"
#include "Types/Pair.hs"
#include "Types/Object.hs"

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

class (Typeable n, Show n, Ord n) => Value n where
    fromVal :: Val -> Eval n
    fromVal = fromVal'
    vCast :: Val -> n
    vCast v@(VRef _)    = castFail v
    vCast v             = doCast v
    castV :: n -> Val
    castV x = VOpaque (MkOpaque x) -- error $ "cannot cast into Val"
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

instance Value [(VStr, Val)] where
     fromVal v = do
         list <- fromVal v
         forM list $ \(k, v) -> do
             str <- fromVal k
             return (str, v)

instance Value VHash where
    fromVal v = do
        list <- fromVal v
        fmap Map.fromList $ forM list $ \(k, v) -> do
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
        hv      <- join $ doHash v hash_fetch
        lns     <- forM (Map.assocs hv) $ \(k, v) -> do
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
type VHash = Map VStr Val
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
    | VOpaque   !VOpaque
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
valType (VCode    c)    = code_iType c
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
valType (VOpaque  _)    = mkType "Object"

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
    { isInvocant    :: !Bool        -- Is it in invocant slot?
    , isOptional    :: !Bool        -- Is it optional?
    , isNamed       :: !Bool        -- Is it named-only?
    , isLValue      :: !Bool        -- Is it lvalue (i.e. not `is copy`)?
    , isWritable    :: !Bool        -- Is it writable (i.e. `is rw`)?
    , isThunk       :: !Bool        -- Is it call-by-name (short-circuit)?
    , paramName     :: !String      -- Parameter name
    , paramContext  :: !Cxt         -- Parameter context: slurpiness and type
    , paramDefault  :: !Exp         -- Default expression when omitted
    }
    deriving (Show, Eq, Ord)

type Params     = [Param]
type Bindings   = [(Param, Exp)]
type SlurpLimit = [(VInt, Exp)]

data VCode = MkCode
    { isMulti       :: !Bool        -- Is this a multi sub/method?
    , subName       :: !String      -- Name of the closure
    , subType       :: !SubType     -- Type of the closure
    , subPad        :: !Pad         -- Lexical pad for sub/method
    , subAssoc      :: !String      -- Associativity
    , subParams     :: !Params      -- Parameters list
    , subBindings   :: !Bindings    -- Currently assumed bindings
    , subSlurpLimit :: !SlurpLimit  -- Max. number of slurpy arguments
    , subReturns    :: !Type        -- Return type
    , subBody       :: !Exp         -- Body of the closure
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
    , subBody = emptyExp
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
    , subBody = emptyExp
    }

instance Ord VComplex where {- ... -}
instance (Typeable a) => Show (TVar a) where
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

type DebugInfo = Maybe (TVar (Map String String))

data Env = Env { envContext :: !Cxt                 -- Current context
               , envLValue  :: !Bool                -- LValue context?
               , envLexical :: !Pad                 -- Lexical pad
               , envGlobal  :: !(TVar Pad)         -- Global pad
               , envClasses :: !ClassTree           -- Current class tree
               , envEval    :: !(Exp -> Eval Val)   -- Active evaluator
               , envCaller  :: !(Maybe Env)         -- Caller's env
               , envBody    :: !Exp                 -- Current AST
               , envDepth   :: !Int                 -- Recursion depth
               , envID      :: !Unique              -- Unique ID of Env
               , envDebug   :: !DebugInfo           -- Debug info map
               , envStash   :: !String              -- Misc. stash
               } deriving (Show, Eq, Ord)

envWant env = 
    showCxt (envContext env) ++ (if envLValue env then ", LValue" else "")
    where
    showCxt CxtVoid         = "Void"
    showCxt (CxtItem typ)   = "Scalar (" ++ showType typ ++ ")"
    showCxt (CxtSlurpy typ) = "List (" ++ showType typ ++ ")"

newtype Pad = MkPad (Map Var [TVar VRef])
    deriving (Eq, Ord, Typeable)

instance Show Pad where
    show pad = "(mkPad [" ++ 
                concat (intersperse ", " $ map dump $ padToList pad) ++
                "])"
        where
        dump (n, ioRefs) = "(" ++ show n ++ ", [" ++
                            concat (intersperse ", " $ map dumpTVar ioRefs) ++
                            "])"
        dumpTVar ioRef = unsafePerformIO $ do
            ref  <- liftSTM $ readTVar ioRef
            dump <- (`runReaderT` undefined) $ (`runContT` return) $ resetT $ do
                dumpRef ref
            return $ "unsafePerformIO (newTVar " ++ vCast dump ++ ")"

-- shiftT = undefined
-- resetT = undefined

mkPad = MkPad . Map.fromList
lookupPad key (MkPad map) = Map.lookup key map
padToList (MkPad map) = Map.assocs map
diffPads (MkPad map1) (MkPad map2) = MkPad $ Map.difference map1 map2
unionPads (MkPad map1) (MkPad map2) = MkPad $ Map.union map1 map2

genMultiSym name ref = do
    ioRef <- liftSTM $ newTVar ref
    return $ \(MkPad map) -> MkPad $ Map.insertWith (++) name [ioRef] map

genSym name ref = do
    ioRef <- liftSTM $ newTVar ref
    return $ \(MkPad map) -> MkPad $ Map.insert name [ioRef] map

show' :: (Show a) => a -> String
show' x = "( " ++ show x ++ " )"

data Scope = SGlobal | SMy | SOur | SLet | STemp | SState
    deriving (Show, Eq, Ord, Read, Enum)

{-
data Eval x
    = EvalIO (ContT Val (ReaderT Env IO) x)
    | EvalSTM (ContT Val (ReaderT Env IO) x)
    deriving (Typeable)
-}

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
        Just ref -> liftSTM $ readTVar ref
        Nothing  -> fail $ "oops, can't find " ++ name

findSym :: String -> Pad -> Maybe (TVar VRef)
findSym name pad = case lookupPad name pad of
    Just (x:_)  -> Just x
    _           -> Nothing

askGlobal :: Eval Pad
askGlobal = do
    glob <- asks envGlobal
    liftSTM $ readTVar glob

writeVar :: Var -> Val -> Eval ()
writeVar name val = do
    glob <- askGlobal
    case findSym name glob of
        Just ioRef -> do
            ref <- liftSTM $ readTVar ioRef
            writeRef ref val
        _        -> return () -- XXX Wrong

readVar :: Var -> Eval Val
readVar name = do
    glob <- askGlobal
    case findSym name glob of
        Just ioRef -> do
            ref <- liftSTM $ readTVar ioRef
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

    sign            =   (char '-' >> return False) 
                    <|> (char '+' >> return True)
                    <|> return True

    decimalLiteral         = number 10 digit        
    hexadecimal     = do{ char 'x'; number 16 hexDigit }
    decimal         = do{ char 'd'; number 10 digit }
    octal           = do{ char 'o'; number 8 octDigit }
    octalBad        = do{ many1 octDigit ; fail "0100 is not octal in perl6 any more, use 0o100 instead." }
    binary          = do{ char 'b'; number 2 (oneOf "01")  }

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
forceRef (MkRef (IScalar sv)) = forceRef =<< fromVal =<< scalar_fetch sv
forceRef (MkRef (IThunk tv)) = thunk_force tv
forceRef r = retError "cannot forceRef" (Val $ VRef r)

dumpRef :: VRef -> Eval Val
dumpRef (MkRef (ICode cv)) = do
    vsub <- code_assuming cv [] []
    return (VStr $ "(MkRef (ICode $ " ++ show vsub ++ "))")
dumpRef (MkRef (IScalar sv)) | scalar_iType sv == mkType "Scalar::Const" = do
    sv <- scalar_fetch sv
    return (VStr $ "(MkRef (IScalar $ " ++ show sv ++ "))")
dumpRef ref = return (VStr $ "(unsafePerformIO . newObject $ mkType \"" ++ show (refType ref) ++ "\")")

readRef :: VRef -> Eval Val
readRef (MkRef (IScalar sv)) = scalar_fetch sv
readRef (MkRef (ICode cv)) = do
    vsub <- code_assuming cv [] []
    return $ VCode vsub
readRef (MkRef (IHash hv)) = do
    pairs <- hash_fetch hv
    return $ VList $ map (\(k, v) -> castV (castV k, v)) (Map.assocs pairs)
readRef (MkRef (IArray av)) = do
    vals <- array_fetch av
    return $ VList vals
readRef (MkRef (IPair pv)) = do
    (k, v) <- pair_fetch pv
    return $ VList [k, v]
readRef (MkRef (IHandle io)) = return . VHandle =<< handle_fetch io
readRef (MkRef (IRule rx)) = return . VRule =<< rule_fetch rx
readRef (MkRef (IThunk tv)) = readRef =<< fromVal =<< thunk_force tv

retIVar :: (Typeable a) => IVar a -> Eval Val 
retIVar = return . VRef . MkRef

writeRef :: VRef -> Val -> Eval ()
writeRef (MkRef (IScalar s)) (VList vals) = do
    av <- liftIO (newArray vals)
    scalar_store s (VRef $ MkRef av)
writeRef (MkRef (IScalar s)) val = scalar_store s val
writeRef (MkRef (IArray s)) val  = array_store s =<< fromVals val
writeRef (MkRef (IHash s)) val   = hash_store s =<< fromVal val
writeRef (MkRef (ICode s)) val   = code_store s =<< fromVal val
writeRef (MkRef (IPair s)) val   = pair_storeVal s val
writeRef (MkRef (IThunk tv)) val = (`writeRef` val) =<< fromVal =<< thunk_force tv
writeRef r _ = retError "cannot writeRef" (Val $ VRef r)

clearRef :: VRef -> Eval ()
clearRef (MkRef (IScalar s)) = scalar_store s undef
clearRef (MkRef (IArray s))  = array_clear s
clearRef (MkRef (IHash s))   = hash_clear s
clearRef (MkRef (IPair s))   = pair_storeVal s undef
clearRef (MkRef (IThunk tv)) = clearRef =<< fromVal =<< thunk_force tv
clearRef r = retError "cannot clearRef" (Val $ VRef r)

newObject :: (MonadIO m) => Type -> m VRef
newObject (MkType "Scalar") = liftSTM $
    fmap scalarRef $ newTVar undef
newObject (MkType "Array")  = liftSTM $
    fmap arrayRef $ (newTVar [] :: STM IArray)
newObject (MkType "Hash")   = liftSTM $
    fmap hashRef $ (newTVar Map.empty :: STM IHash)
newObject (MkType "Code")   = liftSTM $
    fmap codeRef $ newTVar mkSub
newObject typ = fail ("Cannot create object: " ++ showType typ)

-- XXX: Refactor doHash and doArray into one -- also see Eval's [] and {}
doHash :: Val -> (forall a. HashClass a => a -> b) -> Eval b
doHash (VRef (MkRef (IHash hv))) f = return $ f hv
doHash (VRef (MkRef (IScalar sv))) f = do
    val <- scalar_fetch sv
    case val of
        VUndef  -> do
            ref@(MkRef (IHash hv)) <- newObject (MkType "Hash")
            scalar_store sv (VRef ref)
            return $ f hv
        _  -> doHash val f
doHash (VRef (MkRef p@(IPair _))) f = return $ f p
doHash val@(VRef _) _ = retError "Cannot cast into Hash" (Val val)
doHash val f = do
    hv  <- fromVal val
    return $ f (hv :: VHash)

doArray :: Val -> (forall a. ArrayClass a => a -> b) -> Eval b
doArray (VRef (MkRef (IArray hv))) f = return $ f hv
doArray (VRef (MkRef (IScalar sv))) f = do
    val <- scalar_fetch sv
    if defined val
        then doArray val f
        else do
            ref@(MkRef (IArray hv)) <- newObject (MkType "Array")
            scalar_store sv (VRef ref)
            return $ f hv
doArray (VRef (MkRef p@(IPair _))) f = return $ f p
doArray val@(VRef _) _ = retError "Cannot cast into Array" (Val val)
doArray val f = do
    av  <- fromVal val
    return $ f (av :: VArray)

data (Typeable v) => IVar v where
    IScalar :: ScalarClass a => a -> IVar VScalar
    IArray  :: ArrayClass  a => a -> IVar VArray
    IHash   :: HashClass   a => a -> IVar VHash
    ICode   :: CodeClass   a => a -> IVar VCode
    IHandle :: HandleClass a => a -> IVar VHandle
    IRule   :: RuleClass   a => a -> IVar VRule
    IThunk  :: ThunkClass  a => a -> IVar VThunk
    IPair   :: PairClass   a => a -> IVar VPair

data VOpaque where
    MkOpaque :: Value a => a -> VOpaque

instance Eq VOpaque where
    (MkOpaque x) == (MkOpaque y) = castV x == castV y

instance Typeable VOpaque where
    typeOf (MkOpaque x) = typeOf x

instance Ord VOpaque where
    compare x y = castV x `compare` castV y

instance Show VOpaque where
    show (MkOpaque x) = show x

instance Value VOpaque where
    vCast (VOpaque o) = o
    vCast v = MkOpaque v
    castV (MkOpaque x) = castV x

readIVar :: IVar v -> Eval v
readIVar (IScalar x) = scalar_fetch x
readIVar (IPair x)   = pair_fetch x
readIVar (IArray x)  = array_fetch x
readIVar (IHash x)   = hash_fetch x
readIVar _ = error "readIVar"

writeIVar :: IVar v -> v -> Eval ()
writeIVar (IScalar x) = scalar_store x
writeIVar _ = error "writeIVar"

refType (MkRef x) = object_iType x

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
instance Ord (TVar a) where
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
newScalar = liftSTM . (fmap IScalar) . newTVar

newArray :: (MonadIO m) => VArray -> m (IVar VArray)
newArray vals = liftSTM . fmap IArray $ newTVar (map lazyScalar vals)

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


retConstError v = retError "Can't modify constant item" (Val v)

type IArray  = TVar [IVar VScalar]
type IArraySlice = [IVar VScalar]
type IHash   = TVar (Map VStr (IVar VScalar))
type IScalar = TVar Val
type ICode   = TVar VCode
data IHashEnv deriving (Typeable) -- phantom types! fun!
data IScalarCwd deriving (Typeable) -- phantom types! fun!
type IScalarProxy = (Eval VScalar, (VScalar -> Eval ()))
type IScalarLazy = Maybe VScalar

-- these implementation allows no destructions
type IRule   = VRule
type IHandle = VHandle -- XXX maybe TVar?

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

