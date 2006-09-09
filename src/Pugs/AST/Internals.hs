{-# OPTIONS_GHC -cpp -fglasgow-exts -fno-warn-orphans -fallow-overlapping-instances -fallow-undecidable-instances #-}

module Pugs.AST.Internals (
    Eval,      -- uses Val, Env, SIO
    Ann(..),   -- Cxt, Pos, Prag
    Exp(..),   -- uses Pad, Eval, Val
    Env(..),   -- uses Pad, TVar, Exp, Eval, Val
    Val(..),   -- uses V.* (which ones?)
    Value(..), -- uses Val, Eval
    InitDat(..),

    EvalT(..), ContT(..), SubAssoc(..),

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
    ObjectId(..),
    VType, -- uses Type
    VRule(..), -- uses Val

    IVar(..), -- uses *Class and V*
    IArray', IArray, IArraySlice, IHash, IScalar, ICode, IScalarProxy,
    IScalarLazy, IPairHashSlice, IRule, IHandle, IHashEnv(..),
    IScalarCwd(..),

    ArrayClass(..), CodeClass(..), HandleClass(..), HashClass(..),
    ObjectClass(..), PairClass(..), RuleClass(..), ScalarClass(..),
    ThunkClass(..),

    CompUnit(..), mkCompUnit, compUnitVersion,

    -- MonadEval(..),

	transformExp,

    runEvalSTM, runEvalIO, shiftT, resetT, callCC,
    undef, defined, tryIO, guardSTM, guardIO, guardIOexcept,
    readRef, writeRef, clearRef, dumpRef, forceRef,
    askGlobal, writeVar, readVar,
    findSymRef, findSym, valType,
    ifListContext, ifValTypeIsa, evalValType, fromVal', toVV',
    scalarRef, codeRef, arrayRef, hashRef, thunkRef, pairRef,
    newScalar, newArray, newHash, newHandle, newObject,
    proxyScalar, constScalar, lazyScalar, lazyUndef, constArray,
    retError, retControl, retEmpty, retIVar, readIVar, writeIVar,
    fromVals, refType,
    refreshPad, lookupPad, padToList, listToPad,
    mkPrim, mkSub, showRat, showTrueRat,
    cxtOfSigil, cxtOfSigilVar, typeOfSigil, typeOfSigilVar,
    buildParam, defaultArrayParam, defaultHashParam, defaultScalarParam,
	paramsToSig,
    emptyExp,
    isSlurpy, envWant,
    extractPlaceholderVars, fromObject, createObject, createObjectRaw,
    doPair, doHash, doArray,
    unwrap, -- Unwrap(..) -- not used in this file, suitable for factoring out
    newObjectId, runInvokePerl5,
    
    errStrPos, errValPos, enterAtomicEnv, valToBool, envPos', -- for circularity
    expToEvalVal, -- Hack, should be removed once it's figured out how

    DebugInfo, _Sym, _Var, -- String -> ByteString constructors
) where
import Pugs.Internals
import Pugs.Types
import Pugs.Cont hiding (shiftT, resetT)
import qualified Data.Set       as Set
import qualified Data.Map       as Map

import qualified Judy.CollectionsM as C
import qualified Judy.StrMap       as H
import qualified Judy.IntMap       as I
import GHC.Conc (unsafeIOToSTM)

import Pugs.Parser.Number
import Pugs.AST.Eval
import Pugs.AST.Utils
import Pugs.AST.Prag
import Pugs.AST.Pos
import Pugs.AST.Scope
import Pugs.AST.SIO
import Pugs.Embed.Perl5
import qualified Pugs.Val as Val
import qualified Data.ByteString.Char8 as Str

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
import qualified Pugs.Val       as Val

import qualified Judy.CollectionsM as C
import qualified Judy.Hash         as H
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
evalValType (VType t) = return t
evalValType val = return $ valType val

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
Collapse a junction value into a single boolean value.

Works by recursively casting the junction members to booleans, then performing
the actual junction test.
-}
juncToBool :: VJunc -> Eval Bool
juncToBool (MkJunc JAny  _  vs) = do
    bools <- mapM valToBool (Set.elems vs)
    return . isJust $ find id bools
juncToBool (MkJunc JAll  _  vs) = do
    bools <- mapM valToBool (Set.elems vs)
    return . isNothing $ find not bools
juncToBool (MkJunc JNone _  vs) = do
    bools <- mapM valToBool (Set.elems vs)
    return . isNothing $ find id bools
juncToBool (MkJunc JOne ds vs) = do
    bools <- mapM valToBool (Set.elems ds)
    if isJust (find id bools) then return False else do
    bools <- mapM valToBool (Set.elems vs)
    return $ 1 == (length $ filter id bools)

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
Typeclass indicating types that can be converted to\/from 'Val's.

Not to be confused with 'Val' itself, or the 'Exp' constructor @Val@.
-}
class (Typeable n, Show n, Ord n) => Value n where
    fromVal :: Val -> Eval n
    fromVal = fromVal'
    doCast :: Val -> Eval n
{-    doCast v = castFailM v "default implementation of doCast" -}
    fromVV :: Val.Val -> Eval n
    fromVV v = do
        str <- liftSIO (Val.asStr v)
        fail $ "Cannot cast from VV (" ++ cast str ++ ") to " ++ errType (undefined :: n)
    toVV :: n -> Eval Val
    toVV = toVV' . castV
    fromSV :: PerlSV -> Eval n
    fromSV sv = do
        str <- liftIO $ svToVStr sv
        fail $ "Cannot cast from SV (" ++ str ++ ") to " ++ errType (undefined :: n)
    castV :: n -> Val
    castV x = VOpaque (MkOpaque x) -- error $ "Cannot cast into Val"

data VOpaque where
    MkOpaque :: Value a => !a -> VOpaque

fromVal' :: (Value a) => Val -> Eval a
fromVal' (VRef r) = do
    v <- readRef r
    fromVal v
fromVal' (VList vs) | any isRef vs = do
    vs <- forM vs $ \v -> case v of { VRef r -> readRef r; _ -> return v }
    fromVal $ VList vs
    where
    isRef VRef{}    = True
    isRef _         = False
fromVal' (PerlSV sv) = do
    v <- liftIO $ svToVal sv
    case v of
        PerlSV sv'  -> fromSV sv'   -- it was a SV
        val         -> fromVal val  -- it was a Val
fromVal' (VV v) = fromVV v
fromVal' v = doCast v

toVV' :: Val -> Eval Val
toVV' VUndef   = return $ VV $ Val.val $ Val.VUndef $ Val.UUndef
toVV' (VBool v) = return $ VV $ Val.val $ ((cast v) :: Val.PureBit)
toVV' (VInt v) = return $ VV $ Val.val $ ((cast v) :: Val.PureInt)
toVV' (VNum v) = return $ VV $ Val.val $ ((cast v) :: Val.PureNum)
toVV' (VRat v) = return $ VV $ Val.val $ ((cast v) :: Val.PureNum)
toVV' (VStr v) = return $ VV $ Val.val $ ((cast v) :: Val.PureStr)
toVV' x = error $ "don't know how to toVV': " ++ show x

getArrayIndex :: Int -> Maybe (IVar VScalar) -> Eval IArray -> Maybe (Eval b) -> Eval (IVar VScalar)
getArrayIndex idx def getArr _ | idx < 0 = do
    -- first, check if the list is at least abs(idx) long
    MkIArray av s <- getArr
    size <- liftIO $ readIORef s
    if size > (abs (idx+1))
        then do
            e <- liftIO $ C.lookup (idx `mod` size) av
            case e of
                Nothing -> return lazyUndef
                Just x  -> return x
        else errIndex def idx
-- now we are all positive; either extend or return
getArrayIndex idx def getArr ext = do
    let maybeExtend = do case ext of
                             Just doExt -> do { doExt; getArrayIndex idx def getArr Nothing }
                             Nothing    -> errIndex def idx
    MkIArray av s <- getArr
    size <- liftIO $ readIORef s
    if size > idx
        then do
            e <- liftIO $ C.lookup idx av
            case e of
                Just a  -> return a
                Nothing -> return lazyUndef
        else maybeExtend



createObjectRaw :: (MonadSTM m)
    => ObjectId -> Maybe Dynamic -> VType -> [(VStr, Val)] -> m VObject
createObjectRaw uniq opaq typ attrList = do
    attrs   <- liftSTM $ unsafeIOToSTM $ C.fromList $ map (\(a,b) -> (a, lazyScalar b)) attrList
    return $ MkObject
        { objType   = typ
        , objId     = uniq
        , objAttrs  = attrs
        , objOpaque = opaq
        }

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
        fail $ "Cannot cast from " ++ show v ++ " to Object"
    doCast v = castFailM v "VObject"

instance Value VHash where
    fromVal (VObject o) = do
        l <- liftIO $ C.mapToList (\k ivar -> do { v <- readIVar ivar; return (k, v) }) (objAttrs o)
        fmap Map.fromList $ sequence l
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
        { subName     = cast "<anon>"
        , subParams   = [defaultArrayParam]
        , subReturns  = mkType "Scalar::Perl5"
        , subBody     = Prim $ \(args:_) -> do
            svs     <- fromVals args
            runInvokePerl5 sv nullSV svs
        }
    doCast (VCode b) = return b
    doCast (VList [VCode b]) = return b -- XXX Wrong
    doCast v = castFailM v "VCode"

runInvokePerl5 :: PerlSV -> PerlSV -> [PerlSV] -> Eval Val
runInvokePerl5 sub inv args = do 
    env     <- ask
    rv      <- liftIO $ do
        envSV   <- mkVal (VControl $ ControlEnv env)
        invokePerl5 sub inv args envSV (enumCxt $ envContext env)
    case rv of
        Perl5ReturnValues [x]   -> return $ PerlSV x
        Perl5ReturnValues xs    -> return $ VList (map PerlSV xs)
        Perl5ErrorString str    -> fail str
        Perl5ErrorObject err    -> throwError (PerlSV err)

instance Value VBool where
    castV = VBool
    fromSV sv = liftIO $ svToVBool sv
    fromVV vv = liftSIO $ fmap cast (Val.asBit vv)
    doCast (VJunc j)   = juncToBool j
    doCast (VMatch m)  = return $ matchOk m
    doCast (VBool b)   = return $ b
    doCast VUndef      = return $ False
    doCast VType{}     = return $ False
    doCast (VStr "")   = return $ False
    doCast (VStr "0")  = return $ False
    doCast (VInt 0)    = return $ False
    doCast (VRat 0)    = return $ False
    doCast (VNum 0)    = return $ False
    doCast (VList [])  = return $ False
    doCast _           = return $ True


instance Value VInt where
    castV = VInt
    fromVV vv = liftSIO $ fmap cast (Val.asInt vv)
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
    fromVV vv = liftSIO $ fmap cast (Val.asNum vv)
    fromSV sv = liftIO $ svToVNum sv
    doCast VUndef       = return $ 0
    doCast VType{}      = return $ 0
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

instance Value ID where
    castV = VStr . cast
    fromSV sv = fmap cast (liftIO $ svToVStr sv)
    fromVV vv = liftSIO $ cast (Val.asStr vv)
    fromVal = fmap (cast :: VStr -> ID) . fromVal
    doCast = fmap (cast :: VStr -> ID) . doCast

instance Value VStr where
    castV = VStr
    fromSV sv = liftIO $ svToVStr sv
    fromVV vv = liftSIO $ cast (Val.asStr vv)
    fromVal (VList l)    = return . unwords =<< mapM fromVal l
    fromVal v@(PerlSV _) = fromVal' v
    fromVal VUndef       = return ""
    fromVal (VType t)    = return (showType t)
    fromVal v = do
        vt  <- evalValType v
        case showType vt of
            "Pair" -> do
                -- Special case for pairs: "$pair" eq
                -- "$pair.key()\t$pair.value()"
                (k, v)  <- join $ doPair v pair_fetch
                k'      <- fromVal k
                v'      <- fromVal v
                return $ k' ++ "\t" ++ v'
            "Hash" -> do
                --- XXX special case for Hash -- need to Objectify
                hv      <- join $ doHash v hash_fetch
                lns     <- forM (Map.assocs hv) $ \(k, v) -> do
                    str <- fromVal v
                    return $ k ++ "\t" ++ str
                return $ unlines lns
            _ -> fromVal' v
    doCast VUndef        = return ""
    doCast VType{}       = return ""
    doCast (VStr s)      = return s
    doCast (VBool b)     = return $ if b then "1" else ""
    doCast (VInt i)      = return $ show i
    doCast (VRat r)      = return $ showRat r
    doCast (VNum n)      = return $ showNum n
    doCast (VList l)     = fmap unwords (mapM fromVal l)
    doCast (VCode s)     = return $ "<" ++ show (subType s) ++ "(" ++ cast (subName s) ++ ")>"
    doCast (VJunc j)     = return $ show j
    doCast (VThread t)   = return $ takeWhile isDigit $ dropWhile (not . isDigit) $ show t
    doCast (VHandle h)   = return $ "<" ++ "VHandle (" ++ (show h) ++ ">"
    doCast (VMatch m)    = return $ matchStr m
 -- doCast (VType typ)   = return $ showType typ -- "::" ++ showType typ
    doCast (VObject o)   = return $ "<obj:" ++ showType (objType o) ++ ">"
    doCast x             = return $ "<" ++ showType (valType x) ++ ">"


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

instance Value Val.Val where
    fromVal (VV vv) = return vv
    fromVal v       = fromVal =<< toVV v
    fromVV          = return
    toVV            = return . VV
    castV           = VV
    doCast v = castFailM v "VV"

valToStr :: Val -> Eval VStr
valToStr = fromVal

instance Value VList where
    castV = VList
    fromSV sv = return [PerlSV sv]
    fromVV = cast . fmap (map VV . cast) . Val.listVal
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
    fromSV = return . PerlSV
    fromVV = cast . fmap VV . Val.itemVal
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

errStrPos :: VStr -> Pos -> Val
errStrPos str pos = VError (VStr str) [pos]

errValPos :: Val -> Pos -> Val
errValPos val pos = VError val [pos]

enterAtomicEnv :: Env -> Env
enterAtomicEnv env = env{ envAtomic = True }

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
    | VV        !Val.Val
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
valType (VType    t)    = t
valType (VObject  o)    = objType o
valType (VOpaque  _)    = mkType "Object"
valType (PerlSV   _)    = mkType "Scalar::Perl5"
valType (VV       v)    = mkType (cast $ Val.valMeta v)

valToBool :: Val -> Eval VBool
valToBool = fromVal

type VBlock = Exp
data VControl
    = ControlExit  !ExitCode
    | ControlEnv   !Env
-- \| ControlLeave !(Env -> Eval Bool) !Val
    deriving (Show, Eq, Ord, Typeable) -- don't derive YAML for now

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
values, which are henceforth termed "arguments".
-}
data Param = MkOldParam -- "Old" because Pugs.Val.Code defined a new one
    { isInvocant    :: !Bool        -- ^ Is it in invocant slot?
    , isOptional    :: !Bool        -- ^ Is it optional?
    , isNamed       :: !Bool        -- ^ Is it named-only?
    , isLValue      :: !Bool        -- ^ Is it lvalue (i.e. not `is copy`)?
    , isWritable    :: !Bool        -- ^ Is it writable (i.e. `is rw`)?
    , isLazy        :: !Bool        -- ^ Is it call-by-name (short-circuit)?
    , paramName     :: !Var         -- ^ Parameter name
    , paramContext  :: !Cxt         -- ^ Parameter context: slurpiness and type
    , paramDefault  :: !Exp         -- ^ Default expression (to evaluate to)
                                    --     when omitted
    }
    deriving (Show, Eq, Ord, Typeable) {-!derive: YAML_Pos, Perl5, JSON!-}

-- | A list of formal parameters.
type Params     = [Param]

paramToValParam :: Param -> Val.SigParam
paramToValParam param = ret
    where 
    ret = Val.MkParam 
        { Val.p_variable    = v_name $ paramName param  -- XXX sigilization
        , Val.p_types       = []
        , Val.p_constraints = []
        , Val.p_unpacking   = Nothing
        , Val.p_default     = Val.DNil                  -- XXX Exp incompatibility
        , Val.p_label       = v_name $ paramName param  -- XXX sigility
        , Val.p_slots       = Map.empty
        , Val.p_hasAccess   = case param of
                                  MkOldParam { isLValue = True, isWritable = False } -> Val.AccessRO
                                  MkOldParam { isLValue = True, isWritable = True }  -> Val.AccessRW
                                  MkOldParam { isLValue = False }                    -> Val.AccessCopy
        , Val.p_isRef       = Val.p_hasAccess ret == Val.AccessRW
        , Val.p_isLazy      = isLazy param
        , Val.p_isContext   = False -- XXX - not yet handled
        }

paramsToSig :: Params -> Val.Sig
paramsToSig params = 
    Val.SigSubSingle
        { Val.s_requiredPositionalCount =
            length $ filter (\x -> not (isNamed x) && not (isOptional x)) params
        , Val.s_requiredNames =
            Set.fromList $ map (v_name . paramName) $ filter (not . isOptional) params
        , Val.s_positionalList = map paramToValParam $ filter (not . isNamed) params
        , Val.s_namedSet = 
			Map.fromList $ 
				map (\p -> (v_name (paramName p), paramToValParam p)) $ 
				   filter isNamed params
        , Val.s_slurpyScalarList = []  -- XXX unimplemented
        , Val.s_slurpyArray   = Nothing  -- XXX ditto
        , Val.s_slurpyHash    = Nothing  -- XXX yep
        , Val.s_slurpyCode    = Nothing  -- XXX all right
        , Val.s_slurpyCapture = Nothing -- this one is okay as it is ;-)
        }   

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

data SubAssoc
    = ANil | AIrrelevantToParsing | A_left | A_right | A_non | A_chain | A_list 
    deriving (Show, Eq, Ord, Typeable, Data) {-!derive: YAML_Pos, JSON, Perl5 !-}

-- | Represents a sub, method, closure etc. -- basically anything callable.
data VCode = MkCode
    { isMulti       :: !Bool        -- ^ Is this a multi sub\/method?
    , subName       :: !ByteString  -- ^ Name of the closure
    , subType       :: !SubType     -- ^ Type of the closure
    , subEnv        :: !(Maybe Env) -- ^ Lexical pad for sub\/method
    , subAssoc      :: !SubAssoc    -- ^ Associativity
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
    , subName = cast "&"
    , subType = SubPrim
    , subEnv = Nothing
    , subAssoc = ANil
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
    , subName = cast "&"
    , subType = SubBlock
    , subEnv = Nothing
    , subAssoc = ANil
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
    show tv = "<ref:0x" ++ showHex (unsafeCoerce# tv :: Word) ">"

{- Expression annotation
-}
data Ann
    = Cxt !Cxt                -- ^ Context
    | Pos !Pos                -- ^ Position
    | Prag ![Pragma]          -- ^ Lexical pragmas
    | Decl !Scope             -- ^ Within an declarator
    | Parens                  -- ^ Parenthesized
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

_Sym :: Scope -> String -> Exp -> Exp
_Sym scope str exp = Sym scope (cast str) exp

_Var :: String -> Exp
_Var str = Var (cast str)

instance Value Exp where
    {- Val -> Eval Exp -}
    fromVal val = do
        obj <- fromVal val
        return $ fromObject obj
    {- Exp -> Val -}
    {- castV exp = VObject (createObject (mkType "Code::Exp") [("theexp", exp)]) -}
    doCast v = castFailM v "Exp"

-- Recursively apply a transformation to an Exp structure
transformExp :: (Monad m) => (Exp -> m Exp) -> Exp -> m Exp
transformExp f (App a b cs) = do
    a' <- transformExp f a
    b' <- case b of
        Just e -> liftM Just $ transformExp f e
        Nothing -> return Nothing
    cs' <- mapM (transformExp f) cs
    f $ App a' b' cs'
transformExp f (Syn t es) = f =<< liftM (Syn t) (mapM (transformExp f) es)
transformExp f (Ann a e) = f =<< liftM (Ann a) (transformExp f e)
transformExp f (Pad s p e) = f =<< liftM (Pad s p) (transformExp f e)
transformExp f (Sym s v e) = f =<< liftM (Sym s v) (transformExp f e)
transformExp f (Stmts e1 e2) = do 
    e1' <- transformExp f e1
    e2' <- transformExp f e2
    f $ Stmts e1' e2'
transformExp f e = f e

fromObject :: (Typeable a) => VObject -> a
fromObject obj = case objOpaque obj of
    Nothing     -> castFail obj "VObject without opaque"
    Just dyn    -> case fromDynamic dyn of
        Nothing -> castFail obj "VObject's opaque not valueable"
        Just x  -> x

{- FIXME: Figure out how to get this working without a monad, and make it castV -}
expToEvalVal :: Exp -> Eval Val
expToEvalVal exp = do
    obj <- createObject (mkType "Code::Exp") []
    return $ VObject obj{ objOpaque = Just $ toDyn exp }

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

extractPlaceholderVarsExp :: Exp -> ([Exp], [Var]) -> ([Exp], [Var])
extractPlaceholderVarsExp ex (exps, vs) = (ex':exps, vs')
    where
    (ex', vs') = extractPlaceholderVars ex vs

{-| Deduce the placeholder vars ($^a, $^x etc.) used by a block). -}
extractPlaceholderVars :: Exp -> [Var] -> (Exp, [Var])
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
        "when"  -> nub (cast "$_" : vs')
        "given" -> delete (cast "$_") vs'
        _       -> vs'
extractPlaceholderVars (Var var) vs
    | TImplicit <- v_twigil var
    , var' <- var{ v_twigil = TNil }
    = (Var var', nub (var':vs))
    | var == cast "$_"
    = (Var var, nub (var:vs))
    | otherwise
    = (Var var, vs)
extractPlaceholderVars (Ann ann ex) vs = ((Ann ann ex'), vs')
    where
    (ex', vs') = extractPlaceholderVars ex vs
extractPlaceholderVars (Pad scope pad ex) vs = ((Pad scope pad ex'), vs')
    where
    (ex', vs') = extractPlaceholderVars ex vs
extractPlaceholderVars (Sym scope var ex) vs = ((Sym scope var ex'), vs')
    where
    (ex', vs') = extractPlaceholderVars ex vs
extractPlaceholderVars exp vs = (exp, vs)

buildParam :: String -- ^ Type of the parameter
           -> String -- ^ Parameter-sigil (@:@, @!:@, @?@, @!@, etc.)
           -> String -- ^ Name of the parameter (including primary sigil)
           -> Exp    -- ^ Expression for the param's default value
           -> Param
buildParam typ sigil name e = MkOldParam
    { isInvocant    = False
    , isOptional    = '?' `elem` sigil
    , isNamed       = ':' `elem` sigil
    , isLValue      = True
    , isWritable    = (name == "$_")
    , isLazy        = False
    , paramName     = cast name
    , paramContext  = if '*' `elem` sigil
        then CxtSlurpy typ'
        else CxtItem typ'
    , paramDefault  = e
    }
    where
    typ' = if null typ then anyType else mkType typ

defaultArrayParam :: Param
defaultHashParam :: Param
defaultScalarParam :: Param

defaultArrayParam   = buildParam "" "*" "@_" (Val VUndef)
defaultHashParam    = buildParam "" "*" "%_" (Val VUndef)
defaultScalarParam  = buildParam "" "?" "$_" (Var $ cast "$_")

type DebugInfo = Maybe (TVar (Map ID String))

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
    , envPackage :: !Pkg                 -- ^ Current package
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
    , envMaxId   :: !(TVar ObjectId)     -- ^ Current max object id
    , envAtomic  :: !Bool                -- ^ Are we in an atomic transaction?
    } 
    deriving (Show, Eq, Ord, Typeable) -- don't derive YAML for now

envPos' :: Env -> Pos
envPos' = envPos

{-|
Module initialization information.

When a module is loaded and initialized (i.e., its &import routine is
called), it may need to communicate information back to the parser. 
This information is held in a TVar to which the parser has access.
Currently we use this for keeping track of lexical pragma change
requests, but the possiblyExit mechanism may be refactored to use
this as well.
-}
newtype InitDat = MkInitDat
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
'Pugs.Eval.reduce' entry for ('Pad' 'SMy' ...).

The current global and lexical pads are stored in the current 'Env', which
is stored in the @Reader@-monad component of the current 'Eval' monad.
-}

newtype Pad = MkPad { padEntries :: Map Var PadEntry }
    deriving (Eq, Ord, Typeable)

data PadEntry
    = MkEntry !(TVar Bool, TVar VRef)           -- single entry
    | MkEntryMulti ![(TVar Bool, TVar VRef)]    -- multi subs
    deriving (Show, Eq, Ord, Typeable) {-!derive: YAML_Pos!-}

data IHashEnv = MkHashEnv deriving (Show, Typeable) {-!derive: YAML_Pos!-}
data IScalarCwd = MkScalarCwd deriving (Show, Typeable) {-!derive: YAML_Pos!-}

refreshPad :: (MonadIO m, MonadSTM m) => Pad -> m Pad
refreshPad pad = do
    fmap listToPad $ forM (padToList pad) $ \(name, tvars) -> do
        tvars' <- forM tvars $ \orig@(fresh, _) -> do
            isFresh <- liftSTM $ readTVar fresh
            if isFresh then do { liftSTM (writeTVar fresh False); return orig } else do
            -- regen TVar -- this is not the first time entering this scope
            ref <- newObject (typeOfSigilVar name)
            tvar' <- liftSTM $ newTVar ref
            return (fresh, tvar')
        return (name, tvars')

newtype ObjectId = MkObjectId { unObjectId :: Int }
    deriving (Show, Eq, Ord, Typeable) {-!derive: YAML_Pos!-}

data VObject = MkObject
    { objType   :: !VType
    , objAttrs  :: !IHash
    , objOpaque :: !(Maybe Dynamic)
    , objId     :: !ObjectId
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

findSymRef :: Var -> Pad -> Eval VRef
findSymRef name pad = do
    case findSym name pad of
        Just ref -> liftSTM $ readTVar ref
        Nothing  -> fail $ "Cannot find variable: " ++ show name

findSym :: Var -> Pad -> Maybe (TVar VRef)
findSym name pad = case lookupPad name pad of
    Just (x:_)  -> Just x
    _           -> Nothing

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
padToList (MkPad map) = [ (cast k, entryToList v) | (k, v) <- Map.assocs map ]
    where
    entryToList (MkEntry x)         = [x]
    entryToList (MkEntryMulti xs)   = xs

listToPad :: [(Var, [(TVar Bool, TVar VRef)])] -> Pad
listToPad entries = MkPad (Map.fromList [ (cast k, listToEntry v) | (k, v) <- entries ])
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
    { ver  :: Int        -- a version number, currently 1
    --, desc :: String     -- e.g., the name of the contained module
    , pad  :: Pad        -- pad for unit Env
    , ast  :: Exp        -- AST of unit
    } deriving (Show, Eq, Ord, Typeable) {-!derive: YAML_Pos !-}

mkCompUnit :: String -> Pad -> Exp -> CompUnit
mkCompUnit _ pad ast = MkCompUnit compUnitVersion pad ast

compUnitVersion :: Int
compUnitVersion = 5

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
readVar var
    | isGlobalVar var = do
        glob <- askGlobal
        case findSym var glob of
            Just tvar -> do
                ref <- liftSTM $ readTVar tvar
                readRef ref
            _        -> return undef
    | otherwise = do
        lex <- asks envLexical
        case findSym var lex of
            Just tvar -> do
                ref <- liftSTM $ readTVar tvar
                readRef ref
            -- XXX - fallback to global should be eliminated here
            _  -> readVar (toGlobalVar var)

{-|
The \'empty expression\' is just a no-op ('Noop').
-}
emptyExp :: Exp
emptyExp = Noop

retControl :: VControl -> Eval a
retControl c = shiftT $ const (return $ VControl c)

defined :: VScalar -> Bool
defined VUndef  = False
defined VType{} = False
defined _       = True
-- | Produce an undefined Perl 6 value (i.e. 'VUndef').
undef :: VScalar
undef = VUndef

forceRef :: VRef -> Eval Val
forceRef (MkRef (IScalar sv)) = forceRef =<< fromVal =<< scalar_fetch sv
forceRef (MkRef (IThunk tv)) = thunk_force tv
forceRef r = retError "Cannot forceRef" r

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
writeRef r _ = retError "Cannot writeRef" r

clearRef :: VRef -> Eval ()
clearRef (MkRef (IScalar s)) = scalar_store s undef
clearRef (MkRef (IArray s))  = array_clear s
clearRef (MkRef (IHash s))   = hash_clear s
clearRef (MkRef (IPair s))   = pair_storeVal s undef
clearRef (MkRef (IThunk tv)) = clearRef =<< fromVal =<< thunk_force tv
clearRef r = retError "Cannot clearRef" r

newObject :: (MonadSTM m, MonadIO m) => Type -> m VRef
newObject typ = case showType typ of
    "Item"      -> liftSTM $ fmap scalarRef $ newTVar undef
    "Scalar"    -> liftSTM $ fmap scalarRef $ newTVar undef
    "Array"     -> liftIO $ do
        s   <- newIORef 0
        av  <- C.new
        return $ arrayRef (MkIArray av s)
    "Hash"      -> do
        h   <- liftIO (C.new :: IO IHash)
        return $ hashRef h
    "Code"      -> return $! codeRef $ mkPrim
        { subAssoc = ANil
        , subBody  = Prim . const $ fail "Cannot use Undef as a Code object"
        }
    "Type"      -> liftSTM $ fmap scalarRef $ newTVar undef
    "Pair"      -> do
        key <- newObject (mkType "Scalar")
        val <- newObject (mkType "Scalar")
        return $ MkRef (IPair (VRef key, VRef val))
    "Pugs::Internals::VRule"
                -> liftSTM $ fmap scalarRef $ newTVar undef
    _           -> fail ("Cannot create object: " ++ showType typ)

doPair :: Val -> (forall a. PairClass a => a -> b) -> Eval b
doPair (VRef (MkRef (IPair pv))) f = return $ f pv
doPair (VRef (MkRef (IHash hv))) f = do
    vals <- hash_fetch hv
    let [(k, v)] = Map.toList vals
    return $ f (VStr k, v)
doPair (VRef (MkRef (IArray av))) f = do
    vals <- array_fetch av
    let [k, v] = take 2 (vals ++ repeat undef)
    return $ f (k, v)
doPair (VRef (MkRef (IScalar sv))) f = do
    val <- scalar_fetch sv
    case val of
        VUndef  -> do
            ref@(MkRef (IPair pv)) <- newObject (mkType "Pair")
            scalar_store sv (VRef ref)
            return $ f pv
        _  -> doPair val f
doPair (VRef x) _ = retError "Cannot cast into Pair" x
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
            ref@(MkRef (IHash hv)) <- newObject (mkType "Hash")
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
doArray (VRef (MkRef (IArray av))) f = return $ f av
doArray (VRef (MkRef (IScalar sv))) f = do
    val <- scalar_fetch sv
    if defined val
        then doArray val f
        else do
            ref@(MkRef (IArray hv)) <- newObject (mkType "Array")
            scalar_store sv (VRef ref)
            return $ f hv
doArray (VRef (MkRef p@(IPair _))) f = return $ f p
doArray val@(VRef (MkRef IHash{})) f = do
    av  <- fromVal val
    return $ f (av :: VArray)
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
readIVar _ = fail "readIVar"

writeIVar :: IVar v -> v -> Eval ()
writeIVar (IScalar x) = scalar_store x
writeIVar (IArray x) = array_store x
writeIVar (IHash x) = hash_store x
writeIVar _ = fail "writeIVar"

refType :: VRef -> Type
refType (MkRef x) = object_iType x

-- Haddock doesn't seem to like data/instance declarations with a where clause.
#ifndef HADDOCK
instance Eq VRef where
    (==) = const $ const False
instance Ord VRef where
    compare _ _ = EQ
instance Show VRef where
    show ref@(MkRef ivar) = case ivar of
        IScalar x -> showAddr x
        IArray  x -> showAddr x
        IHash   x -> showAddr x
        ICode   x -> showAddr x
        IHandle x -> showAddr x
        IRule   x -> showAddr x
        IThunk  x -> showAddr x
        IPair   x -> showAddr x
        where
        showAddr v = let addr = W# (unsafeCoerce# v)
            in addr `seq` ('<' : showType (refType ref) ++ ":0x" ++ showHex addr ">")

instance Typeable a => Show (IVar a) where
    show ivar = show (MkRef ivar)

instance Eq (IVar a) where
    (==) = const $ const False
instance Ord (IVar a) where
    compare _ _ = EQ
instance Ord (TVar a) where
    compare _ _ = EQ
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

newArray :: (MonadIO m) => VArray -> m (IVar VArray)
newArray vals = liftIO $ do
    --liftSTM $ unsafeIOToSTM $ putStrLn "new array"
    s   <- newIORef (length vals)
    av  <- C.fromList ([0..] `zip` map lazyScalar vals)
    return $ IArray (MkIArray av s)

newHash :: (MonadSTM m) => VHash -> m (IVar VHash)
newHash hash = do
    --liftSTM $ unsafeIOToSTM $ putStrLn "new hash"
    ihash <- liftSTM $ unsafeIOToSTM $ (C.fromList (map (\(a,b) -> (a, lazyScalar b)) (Map.toList hash)) :: IO IHash)
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
type IArray'            = I.IntMap ArrayIndex (IVar VScalar)
data IArray             = MkIArray
    { a_data :: !IArray'
    , a_size :: !(IORef ArrayIndex)
    }
    deriving (Typeable)
type IArraySlice        = [IVar VScalar]
type IHash              = H.StrMap VStr (IVar VScalar) -- XXX UTF8 handled at Types/Hash.hs
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
    maxi <- newTVar $ MkObjectId 1
    return $ MkEnv
        { envContext = CxtVoid
        , envLexical = MkPad Map.empty
        , envImplicit= Map.empty
        , envLValue  = False
        , envGlobal  = glob
        , envPackage = cast "Main"
        , envClasses = initTree
        , envEval    = const (return VUndef)
        , envCaller  = Nothing
        , envOuter   = Nothing
        , envDepth   = 0
        , envBody    = Val undef
        , envDebug   = Just ref -- Set to "Nothing" to disable debugging
        , envPos     = MkPos "<null>" 1 1 1 1
        , envPragmas = []
        , envInitDat = init
        , envMaxId   = maxi
        , envAtomic  = False
        }

fakeEval :: MonadIO m => Eval Val -> m Val
fakeEval = liftIO . runEvalIO _FakeEnv

instance YAML Val.Val
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
instance YAML a => YAML (Map Var a) where
    asYAML x = asYAMLmap "Map" . sortBy (\x y -> fst x `compare` fst y) $
        [ (cast k, asYAML v) | (k, v) <- Map.toList x ]
    fromYAML node = do
        list <- fromYAMLmapBuf node
        return (Map.fromList [ (cast k, v) | (k, v) <- list ])
instance Typeable a => YAML (IVar a) where
    asYAML x = asYAML (MkRef x)
instance YAML VRef where
    asYAML (MkRef (ICode cv)) = do
        VCode vsub  <- fakeEval $ fmap VCode (code_fetch cv)
        vsubC       <- asYAML vsub
        return $ mkTagNode (tagHs "VCode") $ ESeq [vsubC]
    asYAML (MkRef (IScalar sv)) = do
        val <- fakeEval $ scalar_fetch sv
        svC <- asYAML val
        let tag = if scalar_iType sv == mkType "Scalar::Const"
                    then "VScalar" else "IScalar"
        return $ mkTagNode (tagHs tag) $ ESeq [svC]
    asYAML (MkRef (IArray av)) = do
        VList vals <- fakeEval $ fmap VList (array_fetch av)
        avC <- asYAML vals
        return $ mkTagNode (tagHs "Array") $ ESeq [avC]
    asYAML (MkRef (IHash hv)) = do
        VMatch MkMatch{ matchSubNamed = hv } <- fakeEval $ fmap (VMatch . MkMatch False 0 0 "" []) (hash_fetch hv)
        hvC <- asYAML hv
        return $ mkTagNode (tagHs "Hash") $ ESeq [hvC]
    asYAML (MkRef (IPair pv)) = do
        VList [k, v] <- fakeEval $ fmap (\(k, v) -> VList [k, v]) (pair_fetch pv)
        avC <- asYAML (k, v)
        return $ mkTagNode (tagHs "Pair") $ ESeq [avC]
    asYAML ref = do
        val <- fakeEval $ readRef ref
        svC <- asYAML val
        liftIO $ print "====>"
        liftIO $ print svC
        fail ("Not implemented: asYAML \"" ++ showType (refType ref) ++ "\"")
    fromYAML MkNode{n_tag=Just s, n_elem=ESeq [node]}
        | s == packBuf "tag:hs:VCode"   =
            fmap (MkRef . ICode) (fromYAML node :: IO VCode)
        | s == packBuf "tag:hs:VScalar" =
            fmap (MkRef . IScalar) (fromYAML node :: IO VScalar)
        | s == packBuf "tag:hs:Pair"    =
            fmap pairRef (fromYAML node :: IO VPair)
        | s == packBuf "tag:hs:IScalar" = newV newScalar
        | s == packBuf "tag:hs:Array"   = newV newArray
        | s == packBuf "tag:hs:Hash"    = newV newHash
        where newV f = fmap MkRef (f =<< fromYAML node)
    fromYAML node = fail $ "Unhandled YAML node: " ++ show node
instance YAML IHash where
     asYAML x = do
         l <- liftIO $ C.mapToList (\k v -> (k, asYAML v)) x
         asYAMLmap "IHash" l
     fromYAML node = do
         l <- fromYAMLmap node
         l' <- C.fromList l
         return l'

instance YAML ID where
    asYAML x = asYAML (idBuf x)
    fromYAML x = do
        buf <- fromYAML x
        bufToID buf
 
instance Perl5 ID where
    showPerl5 x = showPerl5 (cast x :: ByteString)
instance JSON ID where
    showJSON x = showJSON (cast x :: ByteString)

instance YAML Var where
    asYAML x = asYAML (cast x :: String)
    fromYAML = fmap (cast :: String -> Var) . fromYAML
 
instance Perl5 Var where
    showPerl5 x = showPerl5 (cast x :: String)
instance JSON Var where
    showJSON x = showJSON (cast x :: String)

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

instance Perl5 Val where
    showPerl5 (VUndef) = showP5Class "VUndef"
    showPerl5 (VBool aa) = showP5ArrayObj "VBool" [showPerl5 aa]
    showPerl5 (VInt aa) = showP5ArrayObj "VInt" [showPerl5 aa]
    showPerl5 (VRat aa) = showP5ArrayObj "VRat" [showPerl5 aa]
    showPerl5 (VNum aa) = showP5ArrayObj "VNum" [showPerl5 aa]
    showPerl5 (VStr aa) = showP5ArrayObj "VStr" [showPerl5 aa]
    showPerl5 (VList aa) = showP5ArrayObj "VList" [showPerl5 aa]
    showPerl5 (VType aa) = showP5ArrayObj "VType" [showPerl5 aa]
    showPerl5 (VCode{}) = showP5Class "VUndef" -- XXX - fix the END block!

</DrIFT> Do NOT delete! These instances are your friends! -}

instance Typeable Unique where typeOf _ = typeOf ()
instance Typeable ProcessHandle where typeOf _ = typeOf ()
{- 604 means 6.4.x, not 6.4.0. So please don't delete this. -}
#if __GLASGOW_HASKELL__ <= 604
instance Typeable1 Tree where typeOf1 _ = typeOf ()
#endif

instance Eq VJunc where
    (MkJunc aa ab ac) == (MkJunc aa' ab' ac') = aa == aa' && ab == ab'
                      && ac == ac'

instance Ord VJunc where
    compare (MkJunc aa ab ac) (MkJunc aa' ab' ac') =
            foldl (\x y -> if x == EQ then compare y EQ else x) EQ
            [compare aa aa',compare ab ab',compare ac ac']

{- !!! For DrIFT -- Don't delete !!!

data VJunc = MkJunc
    { juncType :: !JuncType
    , juncDup  :: !(Set Val)
    , juncSet  :: !(Set Val)
    } deriving (Typeable) {-!derive: YAML_Pos!-}

data JuncType = JAny | JAll | JNone | JOne
    deriving (Eq, Ord, Typeable) {-!derive: YAML_Pos!-}

data Scope = SState | SLet | STemp | SEnv | SMy | SOur | SGlobal
    {-!derive: YAML_Pos, JSON, Perl5!-}

data Pad = MkPad { padEntries :: IntMap PadEntry }
    {-!derive: YAML_Pos!-}

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
    {-!derive: JSON!-}

-}

------------------------------------------------------------------------
