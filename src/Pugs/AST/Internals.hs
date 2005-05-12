{-# OPTIONS_GHC -cpp -fglasgow-exts -fno-warn-orphans -funbox-strict-fields #-}
{-# OPTIONS_GHC -#include "UnicodeC.h" #-}

module Pugs.AST.Internals where
import Pugs.Internals
import Pugs.Context
import Pugs.Rule
import Pugs.Types
import Pugs.Cont hiding (shiftT, resetT)
import qualified Data.Set       as Set
import qualified Data.Map       as Map
import qualified Data.IntMap    as IntMap

import Pugs.Parser.Number
import Pugs.AST.Pos
import Pugs.AST.Scope
import Pugs.AST.SIO

#include "../Types/Array.hs"
#include "../Types/Handle.hs"
#include "../Types/Hash.hs"
#include "../Types/Scalar.hs"
#include "../Types/Code.hs"
#include "../Types/Thunk.hs"
#include "../Types/Rule.hs"
#include "../Types/Pair.hs"
#include "../Types/Object.hs"

errIndex :: Show a => Maybe b -> a -> Eval b
errIndex (Just v) _ = return v
errIndex _ idx =
    retError "Modification of non-creatable array value attempted" idx

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

getMapIndex :: Int -> Maybe a -> Eval (IntMap a) -> Maybe (Eval b) -> Eval a
getMapIndex idx def doList _ | idx < 0 = do
    -- first, check if the list is at least abs(idx) long.
    list <- doList
    if IntMap.member (abs (idx+1)) list
        then return . fromJust
            $ IntMap.lookup (idx `mod` (IntMap.size list)) list
        else errIndex def idx
-- now we are all positive; either extend or return
getMapIndex idx def doList ext = do
    list <- doList
    case IntMap.lookup idx list of
        Just a  -> return a
        Nothing -> case ext of
            Just doExt -> do { doExt ; getMapIndex idx def doList Nothing }
            Nothing    -> errIndex def idx

-- |Check whether a 'Val' is of the specified type. Based on the result,
-- either the first or the second evaluation should be performed.
ifValTypeIsa :: Val      -- ^ Value to check the type of
             -> String   -- ^ Name of the type to check against
             -> (Eval a) -- ^ The @then@ case
             -> (Eval a) -- ^ The @else@ case
             -> Eval a
ifValTypeIsa v typ trueM falseM = do
    env <- ask
    vt  <- evalValType v
    if isaType (envClasses env) typ vt
        then trueM
        else falseM

-- |If we are in list context (i.e. 'CxtSlurpy'), then perform the first
-- evaluation; otherwise perform the second.
ifListContext :: (MonadReader Env m)
              => m t -- ^ The @then@ case
              -> m t -- ^ The @else@ case
              -> m t
ifListContext trueM falseM = do
    cxt <- asks envContext
    case cxt of
        CxtSlurpy _   -> trueM
        _           -> falseM

-- |Return the appropriate 'empty' value for the current context -- either
-- an empty list ('VList' []), or undef ('VUndef').
retEmpty :: Eval Val
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
fromVal' v = return $ vCast v
{-do
    rv <- liftIO $ catchJust errorCalls (return . Right $ vCast v) $
        \str -> return (Left str)
    case rv of
        Right v -> return v
        Left e  -> retError e v -- XXX: not working yet
-}

-- |Typeclass indicating types that can be converted to\/from 'Val's.
-- Not to be confused with 'Val' itself, or the 'Exp' constructor @Val@.
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


castFailM :: (Show a, Typeable b) => a -> Eval b
castFailM v = err
    where
    err = fail $ "cannot cast from " ++ show v ++ " to " ++ typ
    typ = tail . dropWhile (not . isSpace) . show $ typeOf err

castFail :: (Show a, Typeable b) => a -> b
castFail v = err
    where
    err = error $ "cannot cast from " ++ show v ++ " to " ++ typ
    typ = show $ typeOf err

instance Value (IVar VScalar) where
    fromVal (VRef (MkRef v@(IScalar _))) = return v
    fromVal (VRef r) = fromVal =<< readRef r
    fromVal v = return $ constScalar v

instance Value VType where
    fromVal (VType t)   = return t
    fromVal v           = return $ valType v

instance Value VMatch where
    fromVal (VRef r) = fromVal =<< readRef r
    fromVal (VMatch m) = return m
    fromVal (VList (x:_)) = fromVal x
    fromVal _ = return $ mkMatchFail

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
    fromVal v       = join $ doPair v pair_fetch

instance Value [(VStr, Val)] where
     fromVal v = do
         list <- fromVal v
         forM list $ \(k, v) -> do
             str <- fromVal k
             return (str, v)

instance Value VHash where
    fromVal (VObject o) = do
        attrs <- liftSTM $ readTVar (objAttrs o)
        fmap Map.fromAscList $ forM (Map.assocs $ attrs) $ \(k, ivar) -> do
            v <- readIVar ivar
            return (k, v)
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
    doCast (VMatch m)  = matchOk m
    doCast (VBool b)   = b
    doCast VUndef      = False
    doCast (VStr "")   = False
    doCast (VStr "0")  = False
    doCast (VInt 0)    = False
    doCast (VRat 0)    = False
    doCast (VNum 0)    = False
    doCast (VList [])  = False
    doCast _           = True

-- |Collapse a junction value into a single boolean value. Works by
-- recursively casting the junction members to booleans, then performing
-- the actual junction test.
juncToBool :: VJunc -> Bool
juncToBool (MkJunc JAny  _  vs) = True `Set.member` Set.map vCast vs
juncToBool (MkJunc JAll  _  vs) = not (False `Set.member` Set.map vCast vs)
juncToBool (MkJunc JNone _  vs) = not (True `Set.member` Set.map vCast vs)
juncToBool (MkJunc JOne  ds vs)
    | True `Set.member` Set.map vCast ds
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
    doCast (VMatch m)   = genericLength $ matchSubPos m
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
    vCast (VCode s)     = "<" ++ show (subType s) ++ "(" ++ subName s ++ ")>"
    vCast (VJunc j)     = show j
    vCast (VThread t)   = takeWhile isDigit $ dropWhile (not . isDigit) $ show t
    vCast (VHandle h)   = "<" ++ "VHandle (" ++ (show h) ++ ">"
    vCast (VMatch m)    = matchStr m
    vCast (VType typ)   = "::" ++ showType typ
    vCast (VObject o)   = "<obj:" ++ showType (objType o) ++ ">"
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

instance Value (VThread Val) where
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
type VType = Type

-- |Represents a value. Note that 'Val' is also a constructor for 'Exp' (i.e.
-- an expression containing a value), so don't confuse the two. Similarly,
-- all the constructors for @data 'Val'@ are themselves puns on the types of
-- values they contain.
data Val
    = VUndef                 -- ^ Undefined value
    | VBool     !VBool       -- ^ Boolean value
    | VInt      !VInt        -- ^ Integer value
    | VRat      !VRat        -- ^ Rational number value
    | VNum      !VNum        -- ^ Number (i.e. a double)
    | VComplex  !VComplex    -- ^ Complex number value
    | VStr      !VStr        -- ^ String value
    | VList     VList        -- ^ List value (lists are lazy, so no '!')
    | VRef      !VRef        -- ^ Reference value
    | VCode     !VCode       -- ^ A code object
    | VBlock    !VBlock
    | VJunc     !VJunc       -- ^ Junction value
    | VError    !VStr !Exp
    | VHandle   !VHandle     -- ^ File handle
    | VSocket   !VSocket     -- ^ Socket handle
    | VThread   !(VThread Val)
    | VProcess  !VProcess    -- ^ PID value
    | VRule     !VRule
    | VSubst    !VSubst      -- ^ Substitution value (correct?)
    | VControl  !VControl
    | VMatch    !VMatch
    | VType     !VType
    | VObject   !VObject
    | VOpaque   !VOpaque
    deriving (Show, Eq, Ord, Typeable)

-- |Find the 'Type' of the value contained by a 'Val'. See "Pugs.Types" for
-- info on types.
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
valType (VMatch   _)    = mkType "Match"
valType (VType    _)    = mkType "Type"
valType (VObject  o)    = objType o
valType (VOpaque  _)    = mkType "Object"

type VBlock = Exp
data VControl
    = ControlLeave !(Env -> Eval Bool) !Val
    | ControlExit  !ExitCode
    | ControlEnv   !Env
    deriving (Show, Eq, Ord)

-- |Represents a junction value.
-- Note that @VJunc@ is also a pun for a 'Val' constructor /containing/ a
-- 'VJunc'.
data VJunc = MkJunc
    { juncType :: !JuncType -- ^ 'JAny', 'JAll', 'JNone' or 'JOne'
    , juncDup  :: !(Set Val)
    -- ^ Only used for @one()@ junctions. Contains those values
    -- that appear more than once (the actual count is
    -- irrelevant), since matching any of these would
    -- automatically violate the 'match /only/ one value'
    -- junctive semantics.
    , juncSet  :: !(Set Val)
    -- ^ Set of values that make up the junction. In @one()@
    -- junctions, contains the set of values that appear exactly
    -- /once/.
    } deriving (Eq, Ord)

-- |The combining semantics of a junction. See 'VJunc' for more info.
data JuncType = JAny  -- ^ Matches if /at least one/ member matches
              | JAll  -- ^ Matches only if /all/ members match
              | JNone -- ^ Matches only if /no/ members match
              | JOne  -- ^ Matches if /exactly one/ member matches
    deriving (Eq, Ord)

instance Show JuncType where
    show JAny  = "any"
    show JAll  = "all"
    show JNone = "none"
    show JOne  = "one"

instance Show VJunc where
    show (MkJunc jtype _ set) =
       	(show jtype) ++ "(" ++
	    (foldl (\x y ->
		if x == "" then (vCast :: Val -> VStr) y
		else x ++ "," ++ (vCast :: Val -> VStr) y)
	    "" $ Set.elems set) ++ ")"

-- |Each 'VCode' structure has a 'SubType' indicating what \'level\' of
-- callable item it is. 'doApply' uses this to figure out how to enter
-- the proper scope and 'Env' when the sub is called.
-- Note that this is the \'type\' of a \'sub\', and has nothing to do with
-- subtyping.
data SubType = SubMethod  -- ^ Method
             | SubRoutine -- ^ Regular subroutine
             | SubBlock   -- ^ Pointy sub or bare block
             | SubPrim    -- ^ Built-in primitive operator (see "Pugs.Prim")
    deriving (Show, Eq, Ord)

isSlurpy :: Param -> Bool
isSlurpy param = isSlurpyCxt $ paramContext param

-- |A formal parameter of a sub (or other callable). These represent
-- declared parameters; don't confuse them with actual parameter values.
data Param = MkParam
    { isInvocant    :: !Bool        -- ^ Is it in invocant slot?
    , isOptional    :: !Bool        -- ^ Is it optional?
    , isNamed       :: !Bool        -- ^ Is it named-only?
    , isLValue      :: !Bool        -- ^ Is it lvalue (i.e. not `is copy`)?
    , isWritable    :: !Bool        -- ^ Is it writable (i.e. `is rw`)?
    , isLazy        :: !Bool        -- ^ Is it call-by-name (short-circuit)?
    , paramName     :: !String      -- ^ Parameter name
    , paramContext  :: !Cxt         -- ^ Parameter context: slurpiness and type
    , paramDefault  :: !Exp         -- ^ Default expression (to evaluate to)
                                    --     when omitted
    }
    deriving (Show, Eq, Ord)

-- |A list of formal parameters.
type Params     = [Param]
-- |A list of bindings from formal parameters ('Param') to actual parameter
-- expressions ('Exp').
type Bindings   = [(Param, Exp)]
type SlurpLimit = [(VInt, Exp)]

-- |Represents a sub, method, closure etc. -- basically anything callable.
data VCode = MkCode
    { isMulti       :: !Bool        -- ^ Is this a multi sub\/method?
    , subName       :: !String      -- ^ Name of the closure
    , subType       :: !SubType     -- ^ Type of the closure
    , subPad        :: !Pad         -- ^ Lexical pad for sub\/method
    , subAssoc      :: !String      -- ^ Associativity
    , subParams     :: !Params      -- ^ Parameters list
    , subBindings   :: !Bindings    -- ^ Currently assumed bindings
    , subSlurpLimit :: !SlurpLimit  -- ^ Max. number of slurpy arguments
    , subReturns    :: !Type        -- ^ Return type
    , subLValue     :: !Bool        -- ^ Is this a lvalue sub?
    , subBody       :: !Exp         -- ^ Body of the closure
    }
    deriving (Show, Eq, Ord, Typeable)

-- |Construct a 'VCode' representing a built-in primitive operator.
-- See "Pugs.Prim" for more info.
mkPrim :: VCode
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
    , subLValue = False
    }

mkSub :: VCode
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
    , subLValue = False
    }

instance Ord VComplex where
    compare (a :+ ai) (b :+ bi) = compare (a, ai) (b, bi)

instance (Typeable a) => Show (TVar a) where
    show _ = "<ref>"

{- Expressions
   "App" represents function application, e.g. myfun($invocant: $arg)

   "Syn" represents a structure that cannot be represented by an App.
   For example, Syn "block" [...block body...]
                Syn "="     [lhs, rhs]
   ... or class definitions, where traits may be assigned either in
   the signature or inside the body.

   There is no top-level marker, like unix filesystems don't have
   volume letters.
-}

-- |Represents an expression tree.
data Exp
    = Noop                              -- ^ No-op
    | App !Exp ![Exp] ![Exp]            -- ^ Function application
                                        --     e.g. myfun($invocant: $arg)
    | Syn !String ![Exp]                -- ^ Syntactic construct that cannot
                                        --     be represented by 'App'.
    | Cxt !Cxt !Exp                     -- ^ Context
    | Pos !Pos !Exp                     -- ^ Position
    | Pad !Scope !Pad !Exp              -- ^ Lexical pad
    | Sym !Scope !Var !Exp              -- ^ Symbol declaration
    | Stmts !Exp !Exp                   -- ^ Multiple statements
    | Prim !([Val] -> Eval Val)         -- ^ Primitive
    | Val !Val                          -- ^ Value
    | Var !Var                          -- ^ Variable
    | NonTerm !Pos                      -- ^ Parse error
     deriving (Show, Eq, Ord, Typeable)

class Unwrap a where
    -- |Unwrap a nested expression, throwing away wrappers (such as 'Cxt' or
    -- 'Pos' to get at the more interesting expression underneath. Works both
    -- on individual 'Exp's, and elementwise on ['Exp']s.
    unwrap :: a -> a
    unwrap = id

instance Unwrap [Exp] where
    unwrap = map unwrap

instance Unwrap Exp where
    unwrap (Cxt _ exp)      = unwrap exp
    unwrap (Pos _ exp)      = unwrap exp
    unwrap (Pad _ _ exp)    = unwrap exp
    unwrap (Sym _ _ exp)    = unwrap exp
    unwrap x                = x

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

-- |(Is this even used? A @grep@ through the sources doesn't find any
-- callers...)
extractExp :: Exp -> ([Exp], [String]) -> ([Exp], [String])
extractExp ex (exps, vs) = (ex':exps, vs')
    where
    (ex', vs') = extract ex vs

-- |(Used by 'extractExp'...)
extract :: Exp -> [String] -> (Exp, [String])
extract (App n invs args) vs = (App n invs' args', vs'')
    where
    (invs', vs')  = foldr extractExp ([], vs) invs
    (args', vs'') = foldr extractExp ([], vs') args
extract (Stmts exp1 exp2) vs = (Stmts exp1' exp2', vs'')
    where
    (exp1', vs')  = extract exp1 vs
    (exp2', vs'') = extract exp2 vs'
extract (Syn n exps) vs = (Syn n exps', vs'')
    where
    (exps', vs') = foldr extractExp ([], vs) exps
    vs'' = case n of
        "when"  -> nub $ vs' ++ ["$_"]
        "given" -> delete "$_" vs'
        _       -> vs'
extract (Var name) vs
    | (sigil:'^':identifer) <- name
    , name' <- (sigil : identifer)
    = (Var name', nub (name':vs))
    | name == "$_"
    = (Var name, nub (name:vs))
    | otherwise
    = (Var name, vs)
extract (Pos pos ex) vs = ((Pos pos ex'), vs')
    where
    (ex', vs') = extract ex vs
extract (Cxt cxt ex) vs = ((Cxt cxt ex'), vs')
    where
    (ex', vs') = extract ex vs
extract exp vs = (exp, vs)

-- can be factored
-- |Return the context implied by a particular primary sigil
-- (\$, \@, \% or \&). E.g. used to find what context to impose on
-- the RHS of a binding (based on the sigil of the LHS).
cxtOfSigil :: Char -> Cxt
cxtOfSigil '$'  = cxtItemAny
cxtOfSigil '@'  = cxtSlurpyAny
cxtOfSigil '%'  = cxtSlurpyAny
cxtOfSigil '&'  = CxtItem $ mkType "Code"
cxtOfSigil '<'  = CxtItem $ mkType "Rule"
cxtOfSigil ':'  = CxtItem $ mkType "Type"
cxtOfSigil x    = internalError $ "cxtOfSigil: unexpected character: " ++ show x

-- |Return the type of variable implied by a name beginning with the specified
-- sigil.
typeOfSigil :: Char -> Type
typeOfSigil '$'  = mkType "Scalar"
typeOfSigil '@'  = mkType "Array"
typeOfSigil '%'  = mkType "Hash"
typeOfSigil '&'  = mkType "Code"
typeOfSigil '<'  = mkType "Rule"
typeOfSigil ':'  = mkType "Type"
typeOfSigil x    = internalError $ "typeOfSigil: unexpected character: " ++ show x

buildParam :: String -- ^ Type of the parameter
           -> String -- ^ Parameter-sigil (@+@ or @?@)
           -> String -- ^ Name of the parameter (including primary sigil)
           -> Exp    -- ^ Expression for the param's default value
           -> Param
buildParam typ sigil name e = MkParam
    { isInvocant    = False
    , isOptional    = (sigil ==) `any` ["?", "+"]
    , isNamed       = (null sigil || head sigil /= '+')
    , isLValue      = True
    , isWritable    = (name == "$_")
    , isLazy        = False
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
defaultScalarParam  = buildParam "" "" "$_" (Val VUndef)

type DebugInfo = Maybe (TVar (Map String String))

-- | Evaluation environment. The current environment is stored in the
-- @Reader@ monad inside the current 'Eval' monad, and can be retrieved using
-- @ask@ for the whole 'Env', or @asks@ if you just want a single field.
data Env = MkEnv
    { envContext :: !Cxt                 -- ^ Current context
                                         -- ('CxtVoid', 'CxtItem' or 'CxtSlurpy')
    , envLValue  :: !Bool                -- ^ Are we in an LValue context?
    , envLexical :: !Pad                 -- ^ Lexical pad for variable lookup
    , envGlobal  :: !(TVar Pad)          -- ^ Global pad for variable lookup
    , envPackage :: !String              -- ^ Current package
    , envClasses :: !ClassTree           -- ^ Current class tree
    , envEval    :: !(Exp -> Eval Val)   -- ^ Active evaluator
    , envCaller  :: !(Maybe Env)         -- ^ Caller's env
    , envBody    :: !Exp                 -- ^ Current AST expression
    , envDepth   :: !Int                 -- ^ Recursion depth
    , envID      :: !Unique              -- ^ Unique ID of Env
    , envDebug   :: !DebugInfo           -- ^ Debug info map
    , envStash   :: !String              -- ^ Misc. stash
    , envPos     :: !Pos                 -- ^ Source position range
    } deriving (Show, Eq, Ord)

envWant :: Env -> String
envWant env =
    showCxt (envContext env) ++ (if envLValue env then ", LValue" else "")
    where
    showCxt CxtVoid         = "Void"
    showCxt (CxtItem typ)   = "Scalar (" ++ showType typ ++ ")"
    showCxt (CxtSlurpy typ) = "List (" ++ showType typ ++ ")"

{- |A 'Pad' keeps track of the names of all currently-bound symbols, and
associates them with the things they actually represent.

It is represented as a mapping from names to /lists/ of bound items.
This is to allow for multi subs, because we will need to keep
/multiple/ subs associated with one symbol. In other cases, the list
should just contain a single value. See 'genSym' and 'genMultiSym' for
more details.

@TVar@ indicates that the mapped-to items are STM transactional variables.

The @Bool@ is a \'freshness\' flag used to ensure that @my@ variable slots
are re-generated each time we enter their scope; see the
'Pugs.Eval.reduce' entry for ('Pad' 'SMy' ... ).

The current global and lexical pads are stored in the current 'Env', which
is stored in the @Reader@-monad component of the current 'Eval' monad.
-}
data Pad = MkPad !(Map Var ([(TVar Bool, TVar VRef)]))
    deriving (Eq, Ord, Typeable)

instance Show Pad where
    show pad = "(mkPad [" ++
                concat (intersperse ", " $ map dump $ padToList pad) ++
                "])"
        where
        dump (n, tvars) = "(" ++ show n ++ ", [" ++
                            concat (intersperse ", " $ map dumpTVar tvars) ++
                            "])"
        dumpTVar (_, tvar) = unsafePerformIO $ do
            ref  <- liftSTM $ readTVar tvar
            dump <- runEvalIO undefined $ dumpRef ref
            return $ "(unsafePerformIO . atomically $ do { bool <- newTVar True; ref <- (newTVar " ++ vCast dump ++ "); return (bool, ref) })"

-- |Produce a 'Pad' from a list of bindings. The inverse of 'padToList'.
-- Not to be confused with the actual 'Pad' constructor @MkPad@.
mkPad :: [(Var, [(TVar Bool, TVar VRef)])] -> Pad
mkPad = MkPad . Map.fromList

-- |Look up a symbol in a 'Pad'.
lookupPad :: Var -- ^ Symbol to look for
          -> Pad -- ^ Pad to look in
          -> Maybe [TVar VRef] -- ^ Might return 'Nothing' if var is not found
lookupPad key (MkPad map) = case Map.lookup key map of
    Just xs -> Just [tvar | (_, tvar) <- xs]
    Nothing -> Nothing

-- |Transform a pad into a flat list of bindings. The inverse of 'mkPad'.
-- Note that @Data.Map.assocs@ returns a list of mappings in ascending key
-- order.
padToList :: Pad -> [(Var, [(TVar Bool, TVar VRef)])]
padToList (MkPad map) = Map.assocs map

-- |Return the difference between two pads.
diffPads :: Pad -- ^ Pad a
         -> Pad -- ^ Pad b
         -> Pad -- ^ a - b
diffPads (MkPad map1) (MkPad map2) = MkPad $ Map.difference map1 map2

unionPads :: Pad -> Pad -> Pad
unionPads (MkPad map1) (MkPad map2) = MkPad $ Map.union map1 map2

type Eval x = EvalT (ContT Val (ReaderT Env SIO)) x
type EvalMonad = EvalT (ContT Val (ReaderT Env SIO))
newtype EvalT m a = EvalT { runEvalT :: m a }

runEvalSTM :: Env -> Eval Val -> STM Val
runEvalSTM env = runSTM . (`runReaderT` env) . (`runContT` return) . runEvalT

runEvalIO :: Env -> Eval Val -> IO Val
runEvalIO env = runIO . (`runReaderT` env) . (`runContT` return) . runEvalT

shiftT :: ((a -> Eval Val) -> Eval Val) -> Eval a
shiftT e = EvalT . ContT $ \k ->
    runContT (runEvalT . e $ lift . lift . k) return

resetT :: Eval Val -> Eval Val
resetT e = lift . lift $
    runContT (runEvalT e) return

instance Monad EvalMonad where
    return a = EvalT $ return a
    m >>= k = EvalT $ do
        a <- runEvalT m
        runEvalT (k a)
    fail str = do
        pos <- asks envPos
        shiftT . const . return $ VError str (NonTerm pos)

instance MonadTrans EvalT where
    lift x = EvalT x

instance Functor EvalMonad where
    fmap f (EvalT a) = EvalT (fmap f a)

instance MonadIO EvalMonad where
    liftIO io = EvalT (liftIO io)

instance MonadSTM EvalMonad where
    -- XXX: Should be this:
    -- liftSTM stm = EvalT (lift . lift . liftSTM $ stm)
    liftSTM stm = EvalT (lift . lift . liftIO . liftSTM $ stm)

instance MonadReader Env EvalMonad where
    ask       = lift ask
    local f m = EvalT $ local f (runEvalT m)

findSymRef :: (MonadSTM m) => String -> Pad -> m VRef
findSymRef name pad = do
    case findSym name pad of
        Just ref -> liftSTM $ readTVar ref
        Nothing  -> fail $ "Cannot find variable: " ++ show name

findSym :: String -> Pad -> Maybe (TVar VRef)
findSym name pad = case lookupPad name pad of
    Just (x:_)  -> Just x
    _           -> Nothing

instance MonadEval EvalMonad

instance MonadCont EvalMonad where
    -- callCC :: ((a -> Eval b) -> Eval a) -> Eval a
    callCC f = EvalT . callCCT $ \c -> runEvalT . f $ \a -> EvalT $ c a

class (MonadReader Env m, MonadCont m, MonadIO m, MonadSTM m) => MonadEval m where
--     askGlobal :: m Pad

askGlobal :: Eval Pad
askGlobal = do
    glob <- asks envGlobal
    liftSTM $ readTVar glob

writeVar :: Var -> Val -> Eval ()
writeVar name val = do
    glob <- askGlobal
    case findSym name glob of
        Just tvar -> do
            ref <- liftSTM $ readTVar tvar
            writeRef ref val
        _        -> return () -- XXX Wrong

readVar :: Var -> Eval Val
readVar name@(_:'*':_) = do
    glob <- askGlobal
    case findSym name glob of
        Just tvar -> do
            ref <- liftSTM $ readTVar tvar
            readRef ref
        _        -> return undef
readVar name@(sigil:rest) = do
    lex <- asks envLexical
    case findSym name lex of
        Just tvar -> do
            ref <- liftSTM $ readTVar tvar
            readRef ref
        _  -> readVar (sigil:'*':rest)
readVar _ = return undef

emptyExp :: Exp
emptyExp = Noop

retControl :: VControl -> Eval a
retControl c = do
    shiftT $ const (return $ VControl c)

retError :: (Show a) => VStr -> a -> Eval b
retError str a = fail $ str ++ ": " ++ show a

defined :: VScalar -> Bool
defined VUndef  = False
defined _       = True
-- |Return an undefined value (i.e. 'VUndef').
undef :: VScalar
undef = VUndef

forceRef :: VRef -> Eval Val
forceRef (MkRef (IScalar sv)) = forceRef =<< fromVal =<< scalar_fetch sv
forceRef (MkRef (IThunk tv)) = thunk_force tv
forceRef r = retError "cannot forceRef" r

dumpRef :: VRef -> Eval Val
dumpRef (MkRef (ICode cv)) = do
    vsub <- code_assuming cv [] []
    return (VStr $ "(MkRef (ICode $ " ++ show vsub ++ "))")
dumpRef (MkRef (IScalar sv)) | scalar_iType sv == mkType "Scalar::Const" = do
    sv <- scalar_fetch sv
    return (VStr $ "(MkRef (IScalar $ " ++ show sv ++ "))")
dumpRef ref = return (VStr $ "(unsafePerformIO . newObject $ mkType \"" ++ showType (refType ref) ++ "\")")

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
    av <- newArray vals
    scalar_store s (VRef $ MkRef av)
writeRef (MkRef (IScalar s)) val = scalar_store s val
writeRef (MkRef (IArray s)) val  = array_store s =<< fromVals val
writeRef (MkRef (IHash s)) val   = hash_store s =<< fromVal val
writeRef (MkRef (ICode s)) val   = code_store s =<< fromVal val
writeRef (MkRef (IPair s)) val   = pair_storeVal s val
writeRef (MkRef (IThunk tv)) val = (`writeRef` val) =<< fromVal =<< thunk_force tv
writeRef r _ = retError "cannot writeRef" r

clearRef :: VRef -> Eval ()
clearRef (MkRef (IScalar s)) = scalar_store s undef
clearRef (MkRef (IArray s))  = array_clear s
clearRef (MkRef (IHash s))   = hash_clear s
clearRef (MkRef (IPair s))   = pair_storeVal s undef
clearRef (MkRef (IThunk tv)) = clearRef =<< fromVal =<< thunk_force tv
clearRef r = retError "cannot clearRef" r

newObject :: (MonadSTM m) => Type -> m VRef
newObject (MkType "Scalar") = liftSTM $
    fmap scalarRef $ newTVar undef
newObject (MkType "Array")  = liftSTM $
    fmap arrayRef $ (newTVar IntMap.empty :: STM IArray)
newObject (MkType "Hash")   = liftSTM $
    fmap hashRef $ (newTVar Map.empty :: STM IHash)
newObject (MkType "Code")   = liftSTM $
    fmap codeRef $ newTVar mkSub
newObject (MkType "Rule")   = liftSTM $
    fmap scalarRef $ newTVar undef
newObject (MkType "Type")   = liftSTM $
    fmap scalarRef $ newTVar undef
newObject typ = fail ("Cannot create object: " ++ showType typ)

doPair :: Val -> (forall a. PairClass a => a -> b) -> Eval b
doPair (VRef (MkRef (IPair pv))) f = return $ f pv
doPair (VRef (MkRef (IScalar sv))) f = do
    val <- scalar_fetch sv
    case val of
        VUndef  -> do
            ref@(MkRef (IPair pv)) <- newObject (MkType "Pair")
            scalar_store sv (VRef ref)
            return $ f pv
        _  -> doPair val f
doPair val@(VRef _) _ = retError "Cannot cast into Pair" val
doPair val f = do
    vs <- fromVal val
    case (vs :: VList) of
        [x, y]  -> return $ f (x, y)
        _       -> do
            pv <- castFailM val
            return $ f (pv :: VPair)

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
doHash (VObject o) f = return $ f (objAttrs o)
doHash (VMatch m) f = do
    return $ f (matchSubNamed m)
doHash val@(VRef _) _ = retError "Cannot cast into Hash" val
doHash val f = do
    hv  <- fromVal val
    return $ f (hv :: VHash)
    {-
    typ <- evalValType val
    cls <- asks envClasses
    if (isaType cls "List" typ)
        then do
            hv  <- fromVal val
            return $ f (hv :: VHash)
        else do
            -- XXX: Fail or return undef?
            -- return $ f (Map.empty :: VHash)
            fail $ "Not an Hash reference: " ++ show val
    -}

-- can be factored out
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
doArray val@(VRef _) _ = retError "Cannot cast into Array" val
doArray (VMatch m) f = do
    return $ f (matchSubPos m)
doArray val f = do
    av  <- fromVal val
    return $ f (av :: VArray)
    {-
    typ <- evalValType val
    cls <- asks envClasses
    if (isaType cls "List" typ)
        then do
            av  <- fromVal val
            return $ f (av :: VArray)
        else do
            -- XXX: Fail or return undef?
            -- return $ f ([] :: VArray)
            fail $ "Not an Array reference: " ++ show val
    -}

-- Haddock doesn't seem to like data/instance declarations with a where clause.
#ifndef HADDOCK
data (Typeable v) => IVar v where
    IScalar :: ScalarClass a => !a -> IVar VScalar
    IArray  :: ArrayClass  a => !a -> IVar VArray
    IHash   :: HashClass   a => !a -> IVar VHash
    ICode   :: CodeClass   a => !a -> IVar VCode
    IHandle :: HandleClass a => !a -> IVar VHandle
    IRule   :: RuleClass   a => !a -> IVar VRule
    IThunk  :: ThunkClass  a => !a -> IVar VThunk
    IPair   :: PairClass   a => !a -> IVar VPair

data VOpaque where
    MkOpaque :: Value a => !a -> VOpaque

data VObject = MkObject
    { objType   :: !VType
    , objAttrs  :: !IHash
    }
    deriving (Show, Eq, Ord, Typeable)

data VMatch = MkMatch
    { matchOk           :: !VBool   -- success?
    , matchFrom         :: !Int     -- .from
    , matchTo           :: !Int     -- .to
    , matchStr          :: !VStr    -- captured str
    , matchSubPos       :: !VList   -- positional submatches
    , matchSubNamed     :: !VHash   -- named submatches
    }
    deriving (Show, Eq, Ord, Typeable)

mkMatchFail = MkMatch False 0 0 "" [] Map.empty
mkMatchOk   = MkMatch True

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
#endif

readIVar :: IVar v -> Eval v
readIVar (IScalar x) = scalar_fetch x
readIVar (IPair x)   = pair_fetch x
readIVar (IArray x)  = array_fetch x
readIVar (IHash x)   = hash_fetch x
readIVar _ = error "readIVar"

writeIVar :: IVar v -> v -> Eval ()
writeIVar (IScalar x) = scalar_store x
writeIVar (IArray x) = array_store x
writeIVar (IHash x) = hash_store x
writeIVar _ = error "writeIVar"

refType :: VRef -> Type
refType (MkRef x) = object_iType x

-- Haddock doesn't seem to like data/instance declarations with a where clause.
#ifndef HADDOCK
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
#endif

scalarRef   :: ScalarClass a=> a -> VRef
scalarRef x = MkRef (IScalar x)
codeRef     :: CodeClass a  => a -> VRef
codeRef x   = MkRef (ICode x)
arrayRef    :: ArrayClass a => a -> VRef
arrayRef x  = MkRef (IArray x)
hashRef     :: HashClass a  => a -> VRef
hashRef x   = MkRef (IHash x)
thunkRef    :: ThunkClass a => a -> VRef
thunkRef x  = MkRef (IThunk x)
pairRef     :: PairClass a  => a -> VRef
pairRef x   = MkRef (IPair x)

newScalar :: (MonadSTM m) => VScalar -> m (IVar VScalar)
newScalar = liftSTM . (fmap IScalar) . newTVar

newArray :: (MonadSTM m) => VArray -> m (IVar VArray)
newArray vals = liftSTM $ do
    av <- newTVar $ IntMap.fromAscList ([0..] `zip` map lazyScalar vals)
    return $ IArray av

newHandle :: (MonadSTM m) => VHandle -> m (IVar VHandle)
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

retConstError :: VScalar -> Eval b
retConstError val = retError "Can't modify constant item" val

-- Haddock doesn't like these; not sure why ...
#ifndef HADDOCK
type IArray             = TVar (IntMap (IVar VScalar))
type IArraySlice        = [IVar VScalar]
type IHash              = TVar (Map VStr (IVar VScalar))
type IScalar            = TVar Val
type ICode              = TVar VCode
type IScalarProxy       = (Eval VScalar, (VScalar -> Eval ()))
type IScalarLazy        = Maybe VScalar
type IPairHashSlice     = (VStr, IVar VScalar)

-- these implementation allows no destructions
type IRule   = VRule
type IHandle = VHandle -- XXX maybe TVar?

data IHashEnv = MkHashEnv deriving (Typeable)
data IScalarCwd = MkScalarCwd deriving (Typeable)

-- GADTs, here we come!
data VRef where
    MkRef   :: (Typeable a) => !(IVar a) -> VRef

instance Typeable VRef where
    typeOf (MkRef x) = typeOf x

instance Typeable1 (EvalT (ContT Val (ReaderT Env SIO))) where
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
#endif
