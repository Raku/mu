{-# OPTIONS_GHC -cpp -fglasgow-exts -fno-warn-orphans -fallow-overlapping-instances -funbox-strict-fields -fallow-undecidable-instances #-}
{-# OPTIONS_GHC -#include "../../UnicodeC.h" #-}

module Pugs.AST.Internals (
    Eval,      -- uses Val, Env, SIO
    Ann(..),   -- Cxt, Pos, Prag
    Exp(..),   -- uses Pad, Eval, Val
    Env(..),   -- uses Pad, TVar, Exp, Eval, Val
    Val(..),   -- uses V.* (which ones?)
    Value(..), -- uses Val, Eval
    InitDat(..),

    EvalT(..), ContT(..),

    Pad(..), PadEntry(..), PadMutator, -- uses Var, TVar, VRef
    Param(..), -- uses Cxt, Exp
    Params, -- uses Param
    Bindings, -- uses Param, Exp
    SlurpLimit, -- VInt, Exp
    
    VRef(..), -- uses IVar
    VOpaque(..), -- uses Value
    VControl(..), -- uses Env, Eval, Val
    VScalar, -- uses Val
    VPair, -- uses Val
    VList, -- uses Val
    VSubst, -- uses VRule, Expr
    VArray, -- uses Val
    VHash, -- uses VStr, Val
    VThunk(..), -- uses Eval, Val
    VProcess(..),
    VMatch(..), mkMatchFail, mkMatchOk, -- uses VList, VHash
    VCode(..), SubType(..), -- uses Pad, Exp, Type
    VJunc(..), JuncType(..), -- uss Val
    VObject(..), -- uses VType, IHash, Unique
    VType, -- uses Type
    VRule(..), -- uses Val

    IVar(..), -- uses *Class and V*
    IArray, IArraySlice, IHash, IScalar, ICode, IScalarProxy,
    IScalarLazy, IPairHashSlice, IRule, IHandle, IHashEnv(..),
    IScalarCwd(..),

    ArrayClass(..), CodeClass(..), HandleClass(..), HashClass(..),
    ObjectClass(..), PairClass(..), RuleClass(..), ScalarClass(..),
    ThunkClass(..),

    CompUnit(..), mkCompUnit,

    -- MonadEval(..),

    runEvalSTM, runEvalIO, shiftT, resetT, callCC,
    undef, defined, tryIO, guardIO, guardIOexcept,
    readRef, writeRef, clearRef, dumpRef, forceRef,
    askGlobal, writeVar, readVar,
    findSymRef, findSym,
    ifListContext, ifValTypeIsa, evalValType, fromVal',
    scalarRef, codeRef, arrayRef, hashRef, thunkRef, pairRef,
    newScalar, newArray, newHash, newHandle, newObject,
    proxyScalar, constScalar, lazyScalar, lazyUndef, constArray,
    retError, retControl, retEmpty, retIVar, readIVar, writeIVar,
    fromVals, refType,
    lookupPad, padToList, listToPad,
    mkPrim, mkSub, showRat, showTrueRat,
    cxtOfSigil, typeOfSigil,
    buildParam, defaultArrayParam, defaultHashParam, defaultScalarParam,
    emptyExp,
    isSlurpy, envWant,
    extractPlaceholderVars, fromObject, createObject,
    doPair, doHash, doArray,
    unwrap, -- Unwrap(..) -- not used in this file, suitable for factoring out
    
    --expToEvalVal, -- Hack, should be removed once it's figured out how
) where
import Pugs.Internals
import Pugs.Types
import Pugs.Cont hiding (shiftT, resetT)
import System.IO.Error (try)
import qualified Data.Set       as Set
import qualified Data.Map       as Map
import qualified Data.IntMap    as IntMap

import Pugs.Parser.Number
import Pugs.AST.Prag
import Pugs.AST.Pos
import Pugs.AST.Scope
import Pugs.AST.SIO
import Pugs.Embed.Perl5

{- <DrIFT> Imports for the DrIFT
import Pugs.AST.Scope
import Pugs.AST.Pos
import Pugs.AST.Prag
import Pugs.AST.SIO
import Pugs.Types
import Pugs.Internals
import Pugs.Embed.Perl5
import qualified Data.Set       as Set
import qualified Data.Map       as Map
 </DrIFT> -}
 
#include "../Types/Array.hs"
#include "../Types/Handle.hs"
#include "../Types/Hash.hs"
#include "../Types/Scalar.hs"
#include "../Types/Code.hs"
#include "../Types/Thunk.hs"
#include "../Types/Rule.hs"
#include "../Types/Pair.hs"
#include "../Types/Object.hs"

-- type Str = Str.FastString

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

{-|
Check whether a 'Val' is of the specified type. Based on the result,
either the first or the second evaluation should be performed.
-}
ifValTypeIsa :: Val      -- ^ Value to check the type of
             -> String   -- ^ Name of the type to check against
             -> (Eval a) -- ^ The @then@ case
             -> (Eval a) -- ^ The @else@ case
             -> Eval a
ifValTypeIsa v (':':typ) trueM falseM = ifValTypeIsa v typ trueM falseM
ifValTypeIsa v typ trueM falseM = do
    env <- ask
    vt  <- evalValType v
    if isaType (envClasses env) typ vt
        then trueM
        else falseM

{-|
If we are in list context (i.e. 'CxtSlurpy'), then perform the first
evaluation; otherwise perform the second.
-}
ifListContext :: (MonadReader Env m)
              => m t -- ^ The @then@ case
              -> m t -- ^ The @else@ case
              -> m t
ifListContext trueM falseM = do
    cxt <- asks envContext
    case cxt of
        CxtSlurpy _ -> trueM
        _           -> falseM

{-|
Return the appropriate 'empty' value for the current context -- either
an empty list ('VList' []), or undef ('VUndef').
-}
retEmpty :: Eval Val
retEmpty = do
    ifListContext
        (return $ VList [])
        (return VUndef)

evalValType :: Val -> Eval Type
evalValType (VRef (MkRef (IScalar sv))) = scalar_type sv
evalValType (VRef r) = return $ refType r
evalValType val = return $ valType val

fromVal' :: (Value a) => Val -> Eval a
fromVal' (VRef r) = do
    v <- readRef r
    fromVal v
fromVal' (VList vs) | not $ null [ undefined | VRef _ <- vs ] = do
    vs <- forM vs $ \v -> case v of { VRef r -> readRef r; _ -> return v }
    fromVal $ VList vs
fromVal' (PerlSV sv) = do
    v <- liftIO $ svToVal sv
    case v of
        PerlSV sv'  -> fromSV sv'   -- it was a SV
        val         -> fromVal val  -- it was a Val
fromVal' v = doCast v

{-|
Typeclass indicating types that can be converted to\/from 'Val's.

Not to be confused with 'Val' itself, or the 'Exp' constructor @Val@.
-}
class (Typeable n, Show n, Ord n) => Value n where
    fromVal :: Val -> Eval n
    fromVal = fromVal'
    doCast :: Val -> Eval n
{-    doCast v = castFailM v "default implementation of doCast" -}
    fromSV :: PerlSV -> Eval n
    fromSV sv = do
        str <- liftIO $ svToVStr sv
        fail $ "cannot cast from SV (" ++ str ++ ") to " ++ errType (undefined :: n)
    castV :: n -> Val
    castV x = VOpaque (MkOpaque x) -- error $ "cannot cast into Val"

errType :: (Typeable a) => a -> String
errType x = show (typeOf x)

createObject :: (MonadIO m) => VType -> [(VStr, Val)] -> m VObject
createObject typ attrList = liftIO $ do
    attrs   <- liftSTM . newTVar . Map.map lazyScalar $ Map.fromList attrList
    uniq    <- newUnique
    return $ MkObject
        { objType   = typ
        , objId     = uniq
        , objAttrs  = attrs
        , objOpaque = Nothing
        }

fromObject :: forall a. (Typeable a) => VObject -> a
fromObject obj = case objOpaque obj of
    Nothing     -> castFail obj "VObject without opaque"
    Just dyn    -> case fromDynamic dyn of
        Nothing -> castFail obj "VObject's opaque not valueable"
        Just x  -> x

castFailM :: forall a b. (Show a, Typeable b) => a -> String -> Eval b
castFailM v str = fail $ "cannot cast from " ++ show v ++ " to " ++ errType (undefined :: b) ++ " (" ++ str ++ ")"

castFail :: forall a b. (Show a, Typeable b) => a -> String -> b
castFail v str = error $ "cannot cast from " ++ show v ++ " to " ++ errType (undefined :: b) ++ " (" ++ str ++ ")"

instance Value (IVar VScalar) where
    fromVal (VRef (MkRef v@(IScalar _))) = return v
    fromVal (VRef r) = fromVal =<< readRef r
    fromVal v = return $ constScalar v
    doCast v = castFailM v "IVar VScalar"

instance Value VType where
    fromVal (VType t)   = return t
    fromVal v@(VObject obj) | objType obj == (mkType "Class") = do
        meta    <- readRef =<< fromVal v
        fetch   <- doHash meta hash_fetchVal
        str     <- fromVal =<< fetch "name"
        return $ mkType str
    fromVal v           = evalValType v
    doCast v = castFailM v "VType"

instance Value VMatch where
    fromVal (VRef r) = fromVal =<< readRef r
    fromVal (VMatch m) = return m
    fromVal (VList (x:_)) = fromVal x
    fromVal _ = return $ mkMatchFail
    doCast v = castFailM v "VMatch"

instance Value VRef where
    fromVal (VRef r)   = return $ r
    fromVal (VList vs) = return $ arrayRef vs
    fromVal v          = return $ scalarRef v
    castV = VRef
    doCast v = castFailM v "VRef"

instance Value [Int] where
    fromVal v = do
        vlist <- fromVal v
        mapM fromVal vlist
    doCast v = castFailM v "[Int]"

instance Value [VStr] where
    castV = VList . map VStr
    fromVal v = do
        vlist <- fromVal v
        mapM fromVal vlist
    doCast v = castFailM v "[VStr]"

instance Value VPair where
    castV pv = VRef $ pairRef pv
    fromVal VUndef  = return (VUndef, VUndef)
    fromVal v       = join $ doPair v pair_fetch
    doCast v = castFailM v "VPair"

instance Value [(VStr, Val)] where
    fromVal v = do
        list <- fromVal v
        forM list $ \(k, v) -> do
            str <- fromVal k
            return (str, v)
    doCast v = castFailM v "[(VStr, Val)]"

instance Value VObject where
    fromVal (VObject o) = return o
    fromVal v@(VRef _) = fromVal' v
    fromVal v = do
        fail $ "cannot cast from " ++ show v ++ " to Object"
    doCast v = castFailM v "VObject"
    {-# NOINLINE castV #-}
    castV v  = VObject obj{ objOpaque = Just $ toDyn v }
        where obj = unsafePerformIO $ createObject (mkType "Code::Exp") []

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
    doCast v = castFailM v "[VPair]"

instance Value [VPair] where
    fromVal v = do
        list <- fromVals v
        doFrom $ concat list
        where
        doFrom :: [Val] -> Eval [VPair]
        doFrom [] = return []
        doFrom (k:v:list) = do
            rest <- doFrom list
            return ((k, v):rest)
        doFrom [k] = do
            -- XXX: warn about odd elements?
            return [(k, undef)]
    doCast v = castFailM v "[VPair]"

instance Value VCode where
    castV = VCode
    fromSV sv = return $ mkPrim
        { subName     = "<anon>"
        , subParams   = [defaultArrayParam]
        , subReturns  = mkType "Scalar::Perl5"
        , subBody     = Prim $ \(args:_) -> do
            svs     <- fromVals args
            env     <- ask
            rv      <- liftIO $ do
                envSV   <- mkVal (VControl $ ControlEnv env)
                invokePerl5 sv nullSV svs envSV (enumCxt $ envContext env)
            return $ case rv of
                [sv]    -> PerlSV sv
                _       -> VList (map PerlSV rv)
        }
    doCast (VCode b) = return b
    doCast (VList [VCode b]) = return b -- XXX Wrong
    doCast v = castFailM v "VCode"

instance Value VBool where
    castV = VBool
    fromSV sv = liftIO $ svToVBool sv
    doCast (VJunc j)   = juncToBool j
    doCast (VMatch m)  = return $ matchOk m
    doCast (VBool b)   = return $ b
    doCast VUndef      = return $ False
    doCast (VStr "")   = return $ False
    doCast (VStr "0")  = return $ False
    doCast (VInt 0)    = return $ False
    doCast (VRat 0)    = return $ False
    doCast (VNum 0)    = return $ False
    doCast (VList [])  = return $ False
    doCast _           = return $ True

{-|
Collapse a junction value into a single boolean value.

Works by recursively casting the junction members to booleans, then performing
the actual junction test.
-}
juncToBool :: VJunc -> Eval Bool
juncToBool (MkJunc JAny  _  vs) = do
    bools <- mapM fromVal (Set.elems vs)
    return . isJust $ find id bools
juncToBool (MkJunc JAll  _  vs) = do
    bools <- mapM fromVal (Set.elems vs)
    return . isNothing $ find not bools
juncToBool (MkJunc JNone _  vs) = do
    bools <- mapM fromVal (Set.elems vs)
    return . isNothing $ find id bools
juncToBool (MkJunc JOne ds vs) = do
    bools <- mapM fromVal (Set.elems ds)
    if isJust (find id bools) then return False else do
    bools <- mapM fromVal (Set.elems vs)
    return $ 1 == (length $ filter id bools)

instance Value VInt where
    castV = VInt
    fromSV sv = liftIO $ svToVInt sv
    doCast (VInt i)     = return $ i
    doCast x            = fmap truncate (fromVal x :: Eval VRat)

instance Value VRat where
    castV = VRat
    fromSV sv = liftIO $ svToVNum sv
    doCast (VInt i)     = return $ i % 1
    doCast (VRat r)     = return $ r
    doCast (VBool b)    = return $ if b then 1 % 1 else 0 % 1
    doCast (VList l)    = return $ genericLength l
    doCast (VStr s) | not (null s) , isSpace $ last s = do
        str <- fromVal (VStr $ init s)
        return str
    doCast (VStr s) | not (null s) , isSpace $ head s = do 
        str <- fromVal (VStr $ tail s)
        return str
    doCast (VStr s)     = return $
        case ( parseNatOrRat s ) of
            Left _   -> 0 % 1
            Right rv -> case rv of
                Left  i -> i % 1
                Right d -> d
    doCast x            = fmap toRational (fromVal x :: Eval VNum)

instance Value VNum where
    castV = VNum
    fromSV sv = liftIO $ svToVNum sv
    doCast VUndef       = return $ 0
    doCast (VBool b)    = return $ if b then 1 else 0
    doCast (VInt i)     = return $ fromIntegral i
    doCast (VRat r)     = return $ realToFrac r
    doCast (VNum n)     = return $ n
    doCast (VStr s) | not (null s) , isSpace $ last s = do
        str <- fromVal (VStr $ init s)
        return str
    doCast (VStr s) | not (null s) , isSpace $ head s = do
        str <- fromVal (VStr $ tail s)
        return str
    doCast (VStr "Inf") = return $ 1/0
    doCast (VStr "-Inf") = return $ -1/0
    doCast (VStr "NaN") = return $ 0/0
    doCast (VStr s)     = return $
        case ( parseNatOrRat s ) of
            Left _   -> 0
            Right rv -> case rv of
                Left  i -> fromIntegral i
                Right d -> realToFrac d
    doCast (VList l)     = return $ genericLength l
    doCast t@(VThread _) = fmap read (fromVal t)
    doCast (VMatch m)    = fromVal (VStr $ matchStr m)
    doCast v = castFailM v "VNum"

instance Value VComplex where
    castV = VComplex
    doCast x            = fmap (:+ 0) (fromVal x :: Eval VNum)

instance Value VStr where
    castV = VStr
    fromSV sv = liftIO $ svToVStr sv
    fromVal (VList l)    = return . unwords =<< mapM fromVal l
    fromVal v@(PerlSV _) = fromVal' v
    fromVal v = do
        vt  <- evalValType v
        case () of
            _
                -- Special case for pairs: "$pair" eq
                -- "$pair.key()\t$pair.value()"
                | vt == mkType "Pair" -> do
                      (k,v) <- join $ doPair v pair_fetch
                      k' <- fromVal k
                      v' <- fromVal v
                      return $ k' ++ "\t" ++ v'
                | vt == mkType "Hash" -> do
                      --- XXX special case for Hash -- need to Objectify
                      hv      <- join $ doHash v hash_fetch
                      lns     <- forM (Map.assocs hv) $ \(k, v) -> do
                          str <- fromVal v
                          return $ k ++ "\t" ++ str
                      return $ unlines lns
                | otherwise -> fromVal' v
    doCast VUndef        = return ""
    doCast (VStr s)      = return s
    doCast (VBool b)     = return $ if b then "1" else ""
    doCast (VInt i)      = return $ show i
    doCast (VRat r)      = return $ showRat r
    doCast (VNum n)      = return $ showNum n
    doCast (VList l)     = fmap unwords (mapM fromVal l)
    doCast (VCode s)     = return $ "<" ++ show (subType s) ++ "(" ++ subName s ++ ")>"
    doCast (VJunc j)     = return $ show j
    doCast (VThread t)   = return $ takeWhile isDigit $ dropWhile (not . isDigit) $ show t
    doCast (VHandle h)   = return $ "<" ++ "VHandle (" ++ (show h) ++ ">"
    doCast (VMatch m)    = return $ matchStr m
    doCast (VType typ)   = return $ showType typ -- "::" ++ showType typ
    doCast (VObject o)   = return $ "<obj:" ++ showType (objType o) ++ ">"
    doCast x             = return $ "<" ++ showType (valType x) ++ ">"

showRat :: VRat -> VStr
showRat r
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

showTrueRat :: VRat -> VStr
showTrueRat r =
    (show n) ++ "/" ++ (show d)
    where
    n = numerator r
    d = denominator r


instance Value [PerlSV] where
    fromVal = fromVals
    doCast v = castFailM v "[PerlSV]"

instance Value PerlSV where
    fromVal (PerlSV sv) = return sv
    fromVal (VStr str) = liftIO $ vstrToSV str
    fromVal (VInt int) = liftIO $ vintToSV int
    fromVal (VRat int) = liftIO $ vnumToSV int
    fromVal (VNum int) = liftIO $ vnumToSV int
    fromVal v = liftIO $ mkValRef v
    doCast v = castFailM v "PerlSV"

showNum :: Show a => a -> String
showNum x
    | str == "Infinity"
    = "Inf"
    | str == "-Infinity"
    = "-Inf"
    | (i, ".0") <- break (== '.') str
    = i -- strip the trailing ".0"
    | otherwise = str
    where
    str = show x

valToStr :: Val -> Eval VStr
valToStr = fromVal

instance Value VList where
    castV = VList
    fromSV sv = return [PerlSV sv]
    fromVal (VRef r) = do
        v <- readRef r
        case v of
            (VList vs) -> return vs
            _          -> return [v]
    fromVal (VList vs) = return vs
    fromVal v = fromVal' v
    doCast (VList l)     = return $ l
    doCast (VUndef)      = return $ [VUndef]
    doCast v             = return $ [v]

instance Value VHandle where
    castV = VHandle
    doCast (VHandle x)  = return $ x
    doCast v = castFailM v "VHandle"

instance Value VSocket where
    castV = VSocket
    doCast (VSocket x)  = return $ x
    doCast v = castFailM v "VSocket"

instance Value (VThread Val) where
    castV = VThread
    doCast (VThread x)  = return $ x
    doCast v = castFailM v "VThread Val"

instance Value VProcess where
    castV = VProcess
    doCast (VProcess x)  = return $ x
    doCast v = castFailM v "VProcess"

instance Value Int where
    fromSV sv = liftIO $ svToVInt sv
    doCast x = intCast x
    castV = VInt . fromIntegral
instance Value Word  where 
    fromVal x = intCast x
    doCast v = castFailM v "Word"
instance Value Word8 where 
    fromVal x = intCast x
    doCast v = castFailM v "Word8"
instance Value [Word8] where
    fromVal val = fmap (map (toEnum . ord)) (fromVal val)
    doCast v = castFailM v "[Word8]"

type VScalar = Val

instance Value VScalar where
    fromSV sv = return $ PerlSV sv
    fromVal (VRef r) = fromVal =<< readRef r
    fromVal v = return v
    doCast v = return v
    castV = id -- XXX not really correct; need to referencify things

intCast :: Num b => Val -> Eval b
intCast x = fmap fromIntegral (fromVal x :: Eval VInt)

type VList = [Val]
type VSubst = (VRule, Exp)
type VArray = [Val]
type VHash = Map VStr Val
data VThunk = MkThunk
    { thunkExp  :: Eval Val
    , thunkType :: VType
    }
    deriving (Typeable) {-!derive: YAML_Pos!-}
newtype VProcess = MkProcess (ProcessHandle)
    deriving (Typeable) {-!derive: YAML_Pos!-}

type VPair = (Val, Val)
type VType = Type

{-|
Representation for rules (i.e. regexes).

Currently there are two types of rules: Perl 5 rules, implemented with PCRE,
and Perl 6 rules, implemented with PGE.
-}
data VRule
    -- | Perl5-compatible regular expression
    = MkRulePCRE
        { rxRegex     :: !Regex -- ^ The \'regular\' expression (as a PCRE
                                --     'Regex' object)
        , rxGlobal    :: !Bool  -- ^ Flag indicating \'global\' (match-all)
        , rxNumSubs   :: !Int   -- ^ The number of subpatterns present.
            , rxStringify :: !Bool
        , rxRuleStr   :: !String -- ^ The rule string, for user reference.
        , rxAdverbs   :: !Val
        }
    -- | Parrot Grammar Engine rule
    | MkRulePGE
        { rxRule      :: !String -- ^ The rule string
        , rxGlobal    :: !Bool   -- ^ Flag indicating \'global\' (match-all)
        , rxStringify :: !Bool
        , rxAdverbs   :: !Val
        }
    deriving (Show, Eq, Ord, Typeable) {-!derive: YAML_Pos!-}

{-|
Represents a value.

Note that 'Val' is also a constructor for 'Exp' (i.e. an expression containing 
a value), so don't confuse the two. Similarly, all the constructors for 
@data 'Val'@ are themselves puns on the types of values they contain.
-}
data Val
    = VUndef                 -- ^ Undefined value
    | VBool     !VBool       -- ^ Boolean value
    | VInt      !VInt        -- ^ Integer value
    | VRat      !VRat        -- ^ Rational number value
    | VNum      !VNum        -- ^ Number (i.e. a double)
    | VComplex  !VComplex    -- ^ Complex number value
    | VStr      !VStr        -- ^ String value
    | VList     !VList       -- ^ List value
    | VType     !VType       -- ^ Type value (e.g. @Int@ or @Type@)
    | VJunc     !VJunc       -- ^ Junction value
    | VError    !Val ![Pos]  -- ^ Error
    | VControl  !VControl
-------------------------------------------------------------------
-- The following are runtime-only values (VRef is negotiable)
    | VRef      !VRef        -- ^ Reference value
    | VCode     !VCode       -- ^ A code object
    | VBlock    !VBlock
    | VHandle   !VHandle     -- ^ File handle
    | VSocket   !VSocket     -- ^ Socket handle
    | VThread   !(VThread Val)
    | VProcess  !VProcess    -- ^ PID value
    | VRule     !VRule       -- ^ Rule\/regex value
    | VSubst    !VSubst      -- ^ Substitution value (correct?)
    | VMatch    !VMatch      -- ^ Match value
    | VObject   !VObject     -- ^ Object
    | VOpaque   !VOpaque
    | PerlSV    !PerlSV
    deriving (Show, Eq, Ord, Typeable) {-!derive: YAML_Pos!-}

{-|
Find the 'Type' of the value contained by a 'Val'.

See "Pugs.Types" for info on types.
-}
valType :: Val -> Type
valType VUndef          = mkType "Scalar"
valType (VRef v)        = refType v
valType (VBool    _)    = mkType "Bool"
valType (VInt     _)    = mkType "Int"
valType (VRat     _)    = mkType "Rat"
valType (VNum     _)    = mkType "Num"
valType (VComplex _)    = mkType "Complex"
valType (VStr     _)    = mkType "Str"
-- valType (VList    _)    = mkType "List"
valType (VList    _)    = mkType "Array"
valType (VCode    c)    = code_iType c
valType (VBlock   _)    = mkType "Block"
valType (VJunc    _)    = mkType "Junction"
valType (VError _ _)    = mkType "Error"
valType (VHandle  _)    = mkType "IO"
valType (VSocket  _)    = mkType "Socket"
valType (VThread  _)    = mkType "Thread"
valType (VProcess _)    = mkType "Process"
valType (VControl _)    = mkType "Control"
valType (VRule    _)    = mkType "Pugs::Internals::VRule"
valType (VSubst   _)    = mkType "Subst"
valType (VMatch   _)    = mkType "Match"
valType (VType    _)    = mkType "Type"
valType (VObject  o)    = objType o
valType (VOpaque  _)    = mkType "Object"
valType (PerlSV   _)    = mkType "Scalar::Perl5"

type VBlock = Exp
data VControl
    = ControlExit  !ExitCode
    | ControlEnv   !Env
-- \| ControlLeave !(Env -> Eval Bool) !Val
    deriving (Show, Eq, Ord, Typeable) -- don't derive YAML for now

{-|
Represents a junction value.

Note that @VJunc@ is also a pun for a 'Val' constructor /containing/ a 'VJunc'.
-}
data VJunc = MkJunc
    { juncType :: !JuncType -- ^ 'JAny', 'JAll', 'JNone' or 'JOne'
    , juncDup  :: !(Set Val)
    -- ^ Only used for @one()@ junctions. Contains those values
    --     that appear more than once (the actual count is
    --     irrelevant), since matching any of these would
    --     automatically violate the 'match /only/ one value'
    --     junctive semantics.
    , juncSet  :: !(Set Val)
    -- ^ Set of values that make up the junction. In @one()@
    --     junctions, contains the set of values that appear exactly
    --     /once/.
    } deriving (Eq, Ord, Typeable) {-!derive: YAML_Pos!-}

-- | The combining semantics of a junction. See 'VJunc' for more info.
data JuncType = JAny  -- ^ Matches if /at least one/ member matches
              | JAll  -- ^ Matches only if /all/ members match
              | JNone -- ^ Matches only if /no/ members match
              | JOne  -- ^ Matches if /exactly one/ member matches
    deriving (Eq, Ord, Typeable) {-!derive: YAML_Pos!-}

instance Show JuncType where
    show JAny  = "any"
    show JAll  = "all"
    show JNone = "none"
    show JOne  = "one"

instance Show VJunc where
    show (MkJunc jtype _ set) =
        (show jtype) ++ "(" ++
            (foldl (\x y ->
                if x == "" then show y
                else x ++ "," ++ show y)
            "" $ Set.elems set) ++ ")"

{-|
Each 'VCode' structure has a 'SubType' indicating what \'level\' of
callable item it is. 'doApply' uses this to figure out how to enter
the proper scope and 'Env' when the sub is called.

Note that this is the \'type\' of a \'sub\', and has nothing to do with
subtyping.
-}
data SubType = SubMethod    -- ^ Method
             | SubCoroutine -- ^ Coroutine
             | SubMacro     -- ^ Macro
             | SubRoutine   -- ^ Regular subroutine
             | SubBlock     -- ^ Bare block
             | SubPointy    -- ^ Pointy sub
             | SubPrim      -- ^ Built-in primitive operator (see "Pugs.Prim")
    deriving (Show, Eq, Ord, Typeable) {-!derive: YAML_Pos, JSON, Perl5!-}

isSlurpy :: Param -> Bool
isSlurpy param = isSlurpyCxt $ paramContext param

{-|
A formal parameter of a sub (or other callable).

These represent declared parameters; don't confuse them with actual parameter 
values.
-}
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
    deriving (Show, Eq, Ord, Typeable) {-!derive: YAML_Pos, Perl5, JSON!-}

-- | A list of formal parameters.
type Params     = [Param]
{-|
A list of bindings from formal parameters ('Param') to actual parameter
expressions ('Exp').
-}
type Bindings   = [(Param, Exp)]
{-|
A sub that has a non-empty 'SlurpLimit' is a bound (or partially bound) sub
that has a finite number of slurpy scalar params bound, and no slurpy array
param bound (see 'VCode' and "Pugs.Bind").

Each list entry consists of the number of slurpable args expected, and an
expression that will evaluate to the actual list of slurpable args.
When the sub is called (see 'Pugs.Eval.apply'), the expression is evaluated.
If it evaluates to /too many/ args, the call will fail.

This needs to be a list (rather than a @Maybe@) because Perl 6's @.assuming@
(i.e. explicit currying) means that a sub can have its arguments bound in
separate stages, and each of the bindings needs to be checked.

>[12:02] <autrijus> scook0: .assuming will impose multiple limits
>[12:02] <autrijus> because you can assume (curry) multiple times
>[12:02] <scook0> ah
>[12:02] <scook0> I'll have to write that in the docs then
>[12:03] <scook0> Am I correct in that they only apply to subs that take a finite number of slurpy scalars?
>[12:04] <scook0> Slurpy array params seem to nuke the SlurpLimit
>[12:04] <scook0> because slurpy arrays can take any number of args
>[12:07] <autrijus> scook0: yes, and yes.
-}
type SlurpLimit = [(VInt, Exp)]

-- | Represents a sub, method, closure etc. -- basically anything callable.
data VCode = MkCode
    { isMulti       :: !Bool        -- ^ Is this a multi sub\/method?
    , subName       :: !String      -- ^ Name of the closure
    , subType       :: !SubType     -- ^ Type of the closure
    , subEnv        :: !(Maybe Env) -- ^ Lexical pad for sub\/method
    , subAssoc      :: !String      -- ^ Associativity
    , subParams     :: !Params      -- ^ Parameters list
    , subBindings   :: !Bindings    -- ^ Currently assumed bindings
    , subSlurpLimit :: !SlurpLimit  -- ^ Max. number of slurpy arguments
    , subReturns    :: !Type        -- ^ Return type
    , subLValue     :: !Bool        -- ^ Is this a lvalue sub?
    , subBody       :: !Exp         -- ^ Body of the closure
    , subCont       :: !(Maybe (TVar VThunk)) -- ^ Coroutine re-entry point
    }
    deriving (Show, Eq, Ord, Typeable) {-!derive: YAML_Pos!-}

{-|
Construct a 'VCode' representing a built-in primitive operator.

See "Pugs.Prim" for more info.
-}
mkPrim :: VCode
mkPrim = MkCode
    { isMulti = True
    , subName = ""
    , subType = SubPrim
    , subEnv = Nothing
    , subAssoc = "pre"
    , subParams = []
    , subBindings = []
    , subSlurpLimit = []
    , subReturns = anyType
    , subBody = emptyExp
    , subLValue = False
    , subCont = Nothing
    }

mkSub :: VCode
mkSub = MkCode
    { isMulti = False
    , subName = ""
    , subType = SubBlock
    , subEnv = Nothing
    , subAssoc = "pre"
    , subParams = []
    , subBindings = []
    , subSlurpLimit = []
    , subReturns = anyType
    , subBody = emptyExp
    , subLValue = False
    , subCont = Nothing
    }

instance Ord VComplex where
    compare (a :+ ai) (b :+ bi) = compare (a, ai) (b, bi)

instance (Typeable a) => Show (TVar a) where
    show _ = "<ref>"

{- Expression annotation
-}
data Ann
    = Cxt !Cxt                -- ^ Context
    | Pos !Pos                -- ^ Position
    | Prag ![Pragma]          -- ^ Lexical pragmas
    deriving (Show, Eq, Ord, Typeable) {-!derive: YAML_Pos!-}

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

-- | Represents an expression tree.
data Exp
    = Noop                              -- ^ No-op
    | App !Exp !(Maybe Exp) ![Exp]      -- ^ Function application
                                        --     e.g. myfun($invocant: $arg)
    | Syn !String ![Exp]                -- ^ Syntactic construct that cannot
                                        --     be represented by 'App'.
    | Ann !Ann !Exp                     -- ^ Annotation (see @Ann@)
    | Pad !Scope !Pad !Exp              -- ^ Lexical pad
    | Sym !Scope !Var !Exp              -- ^ Symbol declaration
    | Stmts !Exp !Exp                   -- ^ Multiple statements
    | Prim !([Val] -> Eval Val)         -- ^ Primitive
    | Val !Val                          -- ^ Value
    | Var !Var                          -- ^ Variable
    | NonTerm !Pos                      -- ^ Parse error
    deriving (Show, Eq, Ord, Typeable) {-!derive: YAML_Pos!-}

instance Value Exp where
    {- Val -> Eval Exp -}
    fromVal val = do
        obj <- fromVal val
        return $ fromObject obj
    {- Exp -> Val -}
    {- castV exp = VObject (createObject (mkType "Code::Exp") [("theexp", exp)]) -}
    doCast v = castFailM v "Exp"
    
 
{-
{- FIXME: Figure out how to get this working without a monad, and make it castV -}
expToEvalVal :: Exp -> Val
--expToEvalVal :: Exp -> Eval Val
expToEvalVal exp = do
    obj <- createObject (mkType "Code::Exp") []
    return $ VObject obj{ objOpaque = Just $ toDyn exp }
-}


class Unwrap a where
    {-|
    Unwrap a nested expression, throwing away wrappers (such as 'Cxt' or
    'Pos' to get at the more interesting expression underneath. Works both
    on individual 'Exp's, and elementwise on ['Exp']s.
    -}
    unwrap :: a -> a
    unwrap = id

instance Unwrap [Exp] where
    unwrap = map unwrap

instance Unwrap Exp where
    unwrap (Ann _ exp)      = unwrap exp
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

extractPlaceholderVarsExp :: Exp -> ([Exp], [String]) -> ([Exp], [String])
extractPlaceholderVarsExp ex (exps, vs) = (ex':exps, vs')
    where
    (ex', vs') = extractPlaceholderVars ex vs

{-| Deduce the placeholder vars ($^a, $^x etc.) used by a block). -}
extractPlaceholderVars :: Exp -> [String] -> (Exp, [String])
extractPlaceholderVars (App n invs args) vs = (App n' invs' args', vs''')
    where
    (n', vs')      = extractPlaceholderVars n vs
    (invs', vs'')  = maybe (invs, vs') (\inv -> let (x, y) = extractPlaceholderVars inv vs' in (Just x, y)) invs
    (args', vs''') = foldr extractPlaceholderVarsExp ([], vs'') args
extractPlaceholderVars (Stmts exp1 exp2) vs = (Stmts exp1' exp2', vs'')
    where
    (exp1', vs')  = extractPlaceholderVars exp1 vs
    (exp2', vs'') = extractPlaceholderVars exp2 vs'
extractPlaceholderVars (Syn n exps) vs = (Syn n exps', vs'')
    where
    (exps', vs') = foldr extractPlaceholderVarsExp ([], vs) exps
    vs'' = case n of
        "when"  -> nub $ vs' ++ ["$_"]
        "given" -> delete "$_" vs'
        _       -> vs'
extractPlaceholderVars (Var name) vs
    | (sigil:'^':identifer) <- name
    , name' <- (sigil : identifer)
    = (Var name', nub (name':vs))
    | name == "$_"
    = (Var name, nub (name:vs))
    | otherwise
    = (Var name, vs)
extractPlaceholderVars (Ann ann ex) vs = ((Ann ann ex'), vs')
    where
    (ex', vs') = extractPlaceholderVars ex vs
extractPlaceholderVars exp vs = (exp, vs)


-- can be factored
{-|
Return the context implied by a particular primary sigil
(\$, \@, \% or \&). E.g. used to find what context to impose on
the RHS of a binding (based on the sigil of the LHS).
-}
cxtOfSigil :: Char -> Cxt
cxtOfSigil '$'  = cxtItemAny
cxtOfSigil '@'  = cxtSlurpyAny
cxtOfSigil '%'  = cxtSlurpyAny
cxtOfSigil '&'  = CxtItem $ mkType "Code"
cxtOfSigil '<'  = CxtItem $ mkType "Pugs::Internals::VRule"
cxtOfSigil ':'  = CxtItem $ mkType "Type"
cxtOfSigil x    = internalError $ "cxtOfSigil: unexpected character: " ++ show x

{-|
Return the type of variable implied by a name beginning with the specified
sigil.
-}
typeOfSigil :: Char -> Type
typeOfSigil '$'  = mkType "Item"
typeOfSigil '@'  = mkType "Array"
typeOfSigil '%'  = mkType "Hash"
typeOfSigil '&'  = mkType "Code"
typeOfSigil '<'  = mkType "Pugs::Internals::VRule"
typeOfSigil ':'  = mkType "Type"
typeOfSigil x    = internalError $ "typeOfSigil: unexpected character: " ++ show x

buildParam :: String -- ^ Type of the parameter
           -> String -- ^ Parameter-sigil (@:@, @!:@, @?@, @!@, etc.)
           -> String -- ^ Name of the parameter (including primary sigil)
           -> Exp    -- ^ Expression for the param's default value
           -> Param
buildParam typ sigil name e = MkParam
    { isInvocant    = False
    , isOptional    = '?' `elem` sigil
    , isNamed       = ':' `elem` sigil
    , isLValue      = True
    , isWritable    = (name == "$_")
    , isLazy        = False
    , paramName     = name
    , paramContext  = if '*' `elem` sigil
        then CxtSlurpy typ'
        else CxtItem typ'
    , paramDefault  = e
    }
    where
    typ' = if null typ then typeOfSigil (head name) else mkType typ

defaultArrayParam :: Param
defaultHashParam :: Param
defaultScalarParam :: Param

defaultArrayParam   = buildParam "" "*" "@_" (Val VUndef)
defaultHashParam    = buildParam "" "*" "%_" (Val VUndef)
defaultScalarParam  = buildParam "" "?" "$_" (Var "$_")

type DebugInfo = Maybe (TVar (Map String String))

{-|
Evaluation environment.

The current environment is stored in the @Reader@ monad inside the current 
'Eval' monad, and can be retrieved using @ask@ for the whole 'Env', or @asks@ 
if you just want a single field.
-}
data Env = MkEnv
    { envContext :: !Cxt                 -- ^ Current context
                                         -- ('CxtVoid', 'CxtItem' or 'CxtSlurpy')
    , envLValue  :: !Bool                -- ^ Are we in an LValue context?
    , envLexical :: !Pad                 -- ^ Lexical pad for variable lookup
    , envImplicit:: !(Map Var ())        -- ^ Set of implicit variables
    , envGlobal  :: !(TVar Pad)          -- ^ Global pad for variable lookup
    , envPackage :: !String              -- ^ Current package
    , envClasses :: !ClassTree           -- ^ Current class tree
    , envEval    :: !(Exp -> Eval Val)   -- ^ Active evaluator
    , envCaller  :: !(Maybe Env)         -- ^ Caller's "env" pad
    , envOuter   :: !(Maybe Env)         -- ^ Outer block's env
    , envBody    :: !Exp                 -- ^ Current AST expression
    , envDepth   :: !Int                 -- ^ Recursion depth
    , envDebug   :: !DebugInfo           -- ^ Debug info map
    , envPos     :: !Pos                 -- ^ Source position range
    , envPragmas :: ![Pragma]            -- ^ List of pragmas in effect
    , envInitDat :: !(TVar InitDat)      -- ^ BEGIN result information
    } 
    deriving (Show, Eq, Ord, Typeable) -- don't derive YAML for now

{-|
Module initialization information.

When a module is loaded and initialized (i.e., its &import routine is
called), it may need to communicate information back to the parser. 
This information is held in a TVar to which the parser has access.
Currently we use this for keeping track of lexical pragma change
requests, but the possiblyExit mechanism may be refactored to use
this as well.
-}
data InitDat = MkInitDat
    { initPragmas :: [Pragma]            -- ^ Pragma values being installed
    } deriving (Show, Eq, Ord, Typeable) {-!derive: YAML_Pos!-}

envWant :: Env -> String
envWant env =
    showCxt (envContext env) ++ (if envLValue env then ", LValue" else "")
    where
    showCxt CxtVoid         = "Void"
    showCxt (CxtItem typ)   = "Scalar (" ++ showType typ ++ ")"
    showCxt (CxtSlurpy typ) = "List (" ++ showType typ ++ ")"

{- Pad -}
{-|
A 'Pad' keeps track of the names of all currently-bound symbols, and
associates them with the things they actually represent.

It is represented as a mapping from names to /lists/ of bound items.
This is to allow for multi subs, because we will need to keep
/multiple/ subs associated with one symbol. In other cases, the list
should just contain a single value. See 'Pugs.AST.genSym' and 'Pugs.AST.genMultiSym' for
more details.

@TVar@ indicates that the mapped-to items are STM transactional variables.

The @Bool@ is a \'freshness\' flag used to ensure that @my@ variable slots
are re-generated each time we enter their scope; see the
'Pugs.Eval.reduce' entry for ('Pad' 'SMy' ... ).

The current global and lexical pads are stored in the current 'Env', which
is stored in the @Reader@-monad component of the current 'Eval' monad.
-}

data Pad = MkPad !(Map Var PadEntry)
    deriving (Eq, Ord, Typeable) {-!derive: YAML_Pos!-}

data PadEntry
    = MkEntry !(TVar Bool, TVar VRef)           -- single entry
    | MkEntryMulti ![(TVar Bool, TVar VRef)]    -- multi subs
    deriving (Show, Eq, Ord, Typeable) {-!derive: YAML_Pos!-}

data IHashEnv = MkHashEnv deriving (Show, Typeable) {-!derive: YAML_Pos!-}
data IScalarCwd = MkScalarCwd deriving (Show, Typeable) {-!derive: YAML_Pos!-}

data VObject = MkObject
    { objType   :: !VType
    , objAttrs  :: !IHash
    , objOpaque :: !(Maybe Dynamic)
    , objId     :: !Unique
    }
    deriving (Show, Eq, Ord, Typeable) {-!derive: YAML_Pos!-}

-- | A '$/' object, the return of a rx match operation.
data VMatch = MkMatch
    { matchOk           :: !VBool   -- success?
    , matchFrom         :: !Int     -- .from
    , matchTo           :: !Int     -- .to
    , matchStr          :: !VStr    -- captured str
    , matchSubPos       :: !VList   -- positional submatches
    , matchSubNamed     :: !VHash   -- named submatches
    }
    deriving (Show, Eq, Ord, Typeable) {-!derive: YAML_Pos!-}


instance Show Pad where
    show pad = "MkPad (padToList " ++ show (padToList pad) ++ ")"

-- | Look up a symbol in a 'Pad', returning the ref it is bound to.
lookupPad :: Var -- ^ Symbol to look for
          -> Pad -- ^ Pad to look in
          -> Maybe [TVar VRef] -- ^ Might return 'Nothing' if var is not found

{-
    We (may) have to fix the name, as the user can write things like
        &::("infix:<+>")(2, 3)
    which, without fixName, wouldn't work, as all operators are currently
    stored as &infix:+, i.e. without the brackets.
-}

lookupPad key (MkPad map) = case Map.lookup (possiblyFixOperatorName key) map of
        Just (MkEntryMulti xs)   -> Just [tvar | (_, tvar) <- xs]
        Just (MkEntry (_, tvar)) -> Just [tvar]
        Nothing -> Nothing

{-|
Transform a pad into a flat list of bindings. The inverse of 'mkPad'.

Note that @Data.Map.assocs@ returns a list of mappings in ascending key order.
-}
padToList :: Pad -> [(Var, [(TVar Bool, TVar VRef)])]
padToList (MkPad map) = (Map.assocs . Map.map entryToList) map
    where
    entryToList (MkEntry x)         = [x]
    entryToList (MkEntryMulti xs)   = xs

listToPad :: [(Var, [(TVar Bool, TVar VRef)])] -> Pad
listToPad = MkPad . Map.map listToEntry . Map.fromList
    where
    listToEntry [x] = MkEntry x
    listToEntry xs  = MkEntryMulti xs

-- | type for a function introducing a change to a Pad
type PadMutator = (Pad -> Pad)

{-|
Serializable compilation unit

See: docs/notes/precompilation_cache.pod
-}
data CompUnit = MkCompUnit
    { ver  :: Int        -- currently 1
    , desc :: String     -- e.g., the name of the contained module
    , glob :: (TVar Pad) -- pad for unit Env
    , ast  :: Exp        -- AST of unit
    } deriving (Show, Eq, Ord, Typeable) {-!derive: YAML_Pos, JSON, Perl5!-}

mkCompUnit = MkCompUnit 1

{- Eval Monad -}
-- type Eval x = EvalT (ContT Val (ReaderT Env SIO)) x
type Eval = EvalT (ContT Val (ReaderT Env SIO))
newtype EvalT m a = EvalT { runEvalT :: m a }

runEvalSTM :: Env -> Eval Val -> STM Val
runEvalSTM env = runSTM . (`runReaderT` env) . (`runContT` return) . runEvalT

runEvalIO :: Env -> Eval Val -> IO Val
runEvalIO env = runIO . (`runReaderT` env) . (`runContT` return) . runEvalT

tryIO :: a -> IO a -> Eval a
tryIO err = lift . liftIO . (`catch` (const $ return err))

{-|
'shiftT' is like @callCC@, except that when you activate the continuation
provided by 'shiftT', it will run to the end of the nearest enclosing 'resetT',
then jump back to just after the point at which you activated the continuation.

Note that because control eventually returns to the point after the 
subcontinuation is activated, you can activate it multiple times in the 
same block. This is unlike @callCC@'s continuations, which discard the current
execution path when activated.

See 'resetT' for an example of how these delimited subcontinuations actually
work.
-}
shiftT :: ((a -> Eval Val) -> Eval Val)
       -- ^ Typically a lambda function of the form @\\esc -> do ...@, where
       --     @esc@ is the current (sub)continuation
       -> Eval a
shiftT e = EvalT . ContT $ \k ->
    runContT (runEvalT . e $ lift . lift . k) return

{-|
Create an scope that 'shiftT'\'s subcontinuations are guaranteed to eventually
exit out the end of.

Consider this example:

> resetT $ do
>     alfa
>     bravo
>     x <- shiftT $ \esc -> do
>        charlie
>        esc 1
>        delta
>        esc 2
>        return 0
>     zulu x

This will:

  1) Perform @alfa@
  
  2) Perform @bravo@
  
  3) Perform @charlie@
  
  4) Bind @x@ to 1, and thus perform @zulu 1@
  
  5) Fall off the end of 'resetT', and jump back to just after @esc 1@
  
  6) Perform @delta@
  
  7) Bind @x@ to 2, and thus perform @zulu 2@
  
  8) Fall off the end of 'resetT', and jump back to just after @esc 2@
  
  6) Escape from the 'resetT', causing it to yield 0

Thus, unlike @callCC@'s continuations, these subcontinuations will eventually
return to the point after they are activated, after falling off the end of the
nearest 'resetT'.
-}
resetT :: Eval Val -- ^ An evaluation, possibly containing a 'shiftT'
       -> Eval Val
resetT e = lift . lift $
    runContT (runEvalT e) return

instance Monad Eval where
    return a = EvalT $ return a
    m >>= k = EvalT $ do
        a <- runEvalT m
        runEvalT (k a)
    fail str = do
        pos <- asks envPos
        shiftT . const . return $ VError (VStr str) [pos]

instance MonadTrans EvalT where
    lift x = EvalT x

instance Functor Eval where
    fmap f (EvalT a) = EvalT (fmap f a)

instance MonadIO Eval where
    liftIO io = EvalT (liftIO io)
    
{-|
Perform an IO action and raise an exception if it fails.
-}
guardIO :: IO a -> Eval a
guardIO io = do
    rv <- liftIO $ try io
    case rv of
        Left e -> fail (show e)
        Right v -> return v

{-|
Like @guardIO@, perform an IO action and raise an exception if it fails.

If the failure matches one of the IOErrors in the 'safetyNet' list,
supress the exception and return an associated value instead.
-}
guardIOexcept :: [((IOError -> Bool), a)] -> IO a -> Eval a
guardIOexcept safetyNet io = do
    rv <- liftIO $ try io
    case rv of
        Right v -> return v
        Left  e -> catcher e safetyNet
    where
    catcher e [] = fail (show e)
    catcher e ((f, res):safetyNets)
        | f e       = return res
        | otherwise = catcher e safetyNets

instance MonadSTM Eval where
    -- XXX: Should be this:
    -- liftSTM stm = EvalT (lift . lift . liftSTM $ stm)
    liftSTM stm = EvalT (lift . lift . liftIO . liftSTM $ stm)

instance MonadReader Env Eval where
    ask       = lift ask
    local f m = EvalT $ local f (runEvalT m)

findSymRef :: (MonadSTM m) => String -> Pad -> m VRef
findSymRef name pad = do
    case findSym name pad of
        Just ref -> liftSTM $ readTVar ref
        Nothing  -> fail $ "Cannot find variable: " ++ show name

findSym :: String -> Pad -> Maybe (TVar VRef)
findSym name pad = fmap head (lookupPad name pad)

instance MonadEval Eval

instance MonadCont Eval where
    -- callCC :: ((a -> Eval b) -> Eval a) -> Eval a
    callCC f = EvalT . callCCT $ \c -> runEvalT . f $ \a -> EvalT $ c a

class (MonadReader Env m, MonadCont m, MonadIO m, MonadSTM m) => MonadEval m
--     askGlobal :: m Pad

{-|
Retrieve the global 'Pad' from the current evaluation environment.

'Env' stores the global 'Pad' in an STM variable, so we have to @asks@
'Eval'\'s @ReaderT@ for the variable, then extract the pad itself from the
STM var.
-}
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

{-|
The \'empty expression\' is just a no-op ('Noop').
-}
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
-- | Produce an undefined Perl 6 value (i.e. 'VUndef').
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

fromVList :: Val -> Eval VArray
fromVList (VList v) = return v
fromVList x = return [x]

fromVHash :: Val -> Eval VHash
fromVHash = fromVal

writeRef :: VRef -> Val -> Eval ()
writeRef (MkRef (IScalar s)) (VList vals) = do
    av <- newArray vals
    scalar_store s (VRef $ MkRef av)
writeRef (MkRef (IScalar s)) val = scalar_store s val
writeRef (MkRef (IArray s)) val  = array_store s =<< fromVList val
writeRef (MkRef (IHash s)) val   = hash_store s =<< fromVHash val
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
newObject (MkType "Item") = liftSTM $
    fmap scalarRef $ newTVar undef
newObject (MkType "Scalar") = liftSTM $
    fmap scalarRef $ newTVar undef
newObject (MkType "Array")  = liftSTM $
    fmap arrayRef $ (newTVar IntMap.empty :: STM IArray)
newObject (MkType "Hash")   = liftSTM $
    fmap hashRef $ (newTVar Map.empty :: STM IHash)
newObject (MkType "Code")   = liftSTM $
    fmap codeRef $ newTVar mkSub
newObject (MkType "Pugs::Internals::VRule")   = liftSTM $
    fmap scalarRef $ newTVar undef
newObject (MkType "Type")   = liftSTM $
    fmap scalarRef $ newTVar undef
newObject (MkType "Pair") = do
    key <- newObject (MkType "Scalar")
    val <- newObject (MkType "Scalar")
    return $ MkRef (IPair (VRef key, VRef val))
newObject typ = fail ("Cannot create object: " ++ showType typ)

doPair :: Val -> (forall a. PairClass a => a -> b) -> Eval b
doPair (VRef (MkRef (IPair pv))) f = return $ f pv
doPair (VRef (MkRef (IArray av))) f = do
    vals <- array_fetch av
    let [k, v] = take 2 (vals ++ repeat undef)
    return $ f (k, v)
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
            pv <- castFailM val "Confusing pair?"
            return $ f (pv :: VPair)

-- XXX: Refactor doHash and doArray into one -- also see Eval's [] and {}
doHash :: Val -> (forall a. HashClass a => a -> b) -> Eval b
doHash (PerlSV sv) f = return $ f sv
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
doArray (PerlSV sv) f = return $ f sv
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

data IVar v where
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

-- | An empty failed match
mkMatchFail :: VMatch
mkMatchFail = MkMatch False 0 0 "" [] Map.empty

-- | Makes a successful match
mkMatchOk :: Int -> Int -> VStr -> VList -> VHash -> VMatch
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
    fromVal (VOpaque o) = return o
    fromVal v = return $ MkOpaque v
    castV (MkOpaque x) = castV x
    doCast v = castFailM v "VOpaque"
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

newHash :: (MonadSTM m) => VHash -> m (IVar VHash)
newHash hash = do
    ihash <- liftSTM (newTVar $ Map.map lazyScalar hash)
    return $ IHash ihash

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

-- GADTs, here we come!
data VRef where
    MkRef   :: (Typeable a) => !(IVar a) -> VRef

instance Typeable VRef where
    typeOf (MkRef x) = typeOf x

instance Typeable1 (EvalT (ContT Val (ReaderT Env SIO))) where
    typeOf1 _ = typeOf ()

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

{- <DrIFT> -- Do NOT delete! These are valuable instances!

{-# NOINLINE _FakeEnv #-}
_FakeEnv :: Env
_FakeEnv = unsafePerformIO $ liftSTM $ do
    ref  <- newTVar Map.empty
    glob <- newTVar $ MkPad Map.empty
    init <- newTVar $ MkInitDat { initPragmas=[] }
    return $ MkEnv
        { envContext = CxtVoid
        , envLexical = MkPad Map.empty
        , envImplicit= Map.empty
        , envLValue  = False
        , envGlobal  = glob
        , envPackage = "main"
        , envClasses = initTree
        , envEval    = const (return VUndef)
        , envCaller  = Nothing
        , envOuter   = Nothing
        , envDepth   = 0
        -- XXX see AST/Internals.hs
        --, envID      = uniq
        , envBody    = Val undef
        , envDebug   = Just ref -- Set to "Nothing" to disable debugging
        , envPos     = MkPos "<null>" 1 1 1 1
        , envPragmas = []
        , envInitDat = init
        }

fakeEval :: MonadIO m => Eval Val -> m Val
fakeEval = liftIO . runEvalIO _FakeEnv


instance YAML ([Val] -> Eval Val) where
    asYAML _ = return nilNode
    fromYAML _ = return (const $ return VUndef)
instance YAML (Maybe Env) where
    asYAML _ = return nilNode
    fromYAML _ = return Nothing
instance YAML (Eval Val) where
    asYAML x = asYAML =<< fakeEval x
    fromYAML x = return =<< fromYAML x
instance YAML a => YAML (Map String a) where
    asYAML x = asYAMLmap "Map" $ Map.toAscList (Map.map asYAML x)
    fromYAML node = fmap Map.fromList (fromYAMLmap node)
instance Typeable a => YAML (IVar a) where
    asYAML x = asYAML (MkRef x)
instance YAML VRef where
    asYAML (MkRef (ICode cv)) = do
        VCode vsub  <- fakeEval $ fmap VCode (code_fetch cv)
        vsubC       <- asYAML vsub
        return $ mkTagNode (tagHs "VCode") $ YamlSeq [vsubC]
    asYAML (MkRef (IScalar sv)) = do
        val <- fakeEval $ scalar_fetch sv
        svC <- asYAML val
        let tag = if scalar_iType sv == mkType "Scalar::Const"
                    then "VScalar" else "IScalar"
        return $ mkTagNode (tagHs tag) $ YamlSeq [svC]
    asYAML (MkRef (IArray av)) = do
        VList vals <- fakeEval $ fmap VList (array_fetch av)
        avC <- asYAML vals
        return $ mkTagNode (tagHs "Array") $ YamlSeq [avC]
    asYAML (MkRef (IHash hv)) = do
        VMatch MkMatch{ matchSubNamed = hv } <- fakeEval $ fmap (VMatch . MkMatch False 0 0 "" []) (hash_fetch hv)
        hvC <- asYAML hv
        return $ mkTagNode (tagHs "Hash") $ YamlSeq [hvC]
    asYAML (MkRef (IPair pv)) = do
        VList [k, v] <- fakeEval $ fmap (\(k, v) -> VList [k, v]) (pair_fetch pv)
        avC <- asYAML (k, v)
        return $ mkTagNode (tagHs "Pair") $ YamlSeq [avC]
    asYAML ref = do
        val <- fakeEval $ readRef ref
        svC <- asYAML val
        liftIO $ print "====>"
        liftIO $ print svC
        fail ("not implemented: asYAML \"" ++ showType (refType ref) ++ "\"")
    fromYAML MkYamlNode{tag=Just s, el=YamlSeq [node]}
        | s == Str.pack "tag:hs:VCode"   =
            fmap (MkRef . ICode) (fromYAML node :: IO VCode)
        | s == Str.pack "tag:hs:VScalar" =
            fmap (MkRef . IScalar) (fromYAML node :: IO VScalar)
        | s == Str.pack "tag:hs:Pair"    =
            fmap pairRef (fromYAML node :: IO VPair)
        | s == Str.pack "tag:hs:IScalar" = newV newScalar
        | s == Str.pack "tag:hs:Array"   = newV newArray
        | s == Str.pack "tag:hs:Hash"    = newV newHash
        where newV f = fmap MkRef (f =<< fromYAML node)
    fromYAML node = fail $ "unhandled node: " ++ show node

instance YAML VControl
instance YAML (Set Val)
instance YAML (VThread Val)
instance YAML ClassTree
instance YAML Dynamic
instance YAML Pragma
instance YAML ProcessHandle
instance YAML Regex
instance YAML Unique
instance YAML VComplex
instance YAML VHandle
instance YAML VOpaque
instance YAML VSocket
instance YAML PerlSV

instance Perl5 Exp where
    showPerl5 _ = "(undef)"
instance JSON Exp where
    showJSON _ = "null"

-- Non-canonical serialization... needs work
instance (Show (TVar a)) => Perl5 (TVar a) where
    showPerl5 _ = "(warn '<ref>')"
instance (Show (TVar a)) => JSON (TVar a) where
    showJSON _ = "null"
</DrIFT> Do NOT delete! These instances are your friends! -}

instance Typeable Unique where typeOf _ = typeOf ()
instance Typeable ProcessHandle where typeOf _ = typeOf ()
instance Typeable Regex where typeOf _ = typeOf ()
instance Typeable1 Tree where typeOf1 _ = typeOf ()



{- !!! For DrIFT -- Don't delete !!!

data Scope = SState | SLet | STemp | SEnv | SMy | SOur | SGlobal
    {-!derive: YAML_Pos, JSON, Perl5!-}

data Pos = MkPos
    { posName           :: !String, posBeginLine      :: !Int
    , posBeginColumn    :: !Int
    , posEndLine        :: !Int
    , posEndColumn      :: !Int
    }
    {-!derive: YAML_Pos, JSON, Perl5!-}

data Type
    = MkType !String      -- ^ A regular type
    | TypeOr  !Type !Type -- ^ The disjunction (|) of two types
    | TypeAnd !Type !Type -- ^ The conjunction (&) of two types
    {-!derive: YAML_Pos, JSON, Perl5!-}

data Cxt = CxtVoid | CxtItem !Type | CxtSlurpy !Type
    {-!derive: YAML_Pos, JSON, Perl5!-}

data Val
    = VUndef                 -- ^ Undefined value
    | VBool     !VBool       -- ^ Boolean value
    | VInt      !VInt        -- ^ Integer value
    | VRat      !VRat        -- ^ Rational number value
    | VNum      !VNum        -- ^ Number (i.e. a double)
    | VStr      !VStr        -- ^ String value
    | VList     !VList       -- ^ List value
    | VType     !VType       -- ^ Type value (e.g. @Int@ or @Type@)
    {-!derive: Perl5, JSON!-}

-}

------------------------------------------------------------------------
