{-# OPTIONS_GHC -cpp -fglasgow-exts -fno-warn-orphans -fallow-overlapping-instances -funbox-strict-fields -fallow-undecidable-instances #-}

-- This is WIP towards an overhaul of Pugs.AST.Internals.
-- It includes a new and more detailed AST which captures much more Perl 6
-- semantics; captures; MOP based on Stevan++'s Moose.pm; and cleans up
-- some accrued cruft.

module Pugs.AST.Internals (
    Eval,      -- uses Val, Env, SIO
    Ann(..),   -- Cxt, Pos, Prag
    Exp(..),   -- uses Pad, Eval, Val
    Env(..),   -- uses Pad, TVar, Exp, Eval, Val
    Val(..),   -- uses V.* (which ones?)
    Value(..), -- uses Val, Eval
    InitDat(..),

    EvalT(..), ContT(..),

    Pad(..), PadEntry(..), PadMutator, -- uses Var, TVar
    Param(..), -- uses Cxt, Exp
    Params, -- uses Param
    Bindings, -- uses Param, Exp
    SlurpLimit, -- VInt, Exp
    
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
    VObject(..), -- uses IHash, Unique
    ObjectId(..),
    VRule(..), -- uses Val

    IVar(..), -- uses *Class and V*
    IArray, IArraySlice, IHash, IScalar, ICode, IScalarProxy,
    IScalarLazy, IPairHashSlice, IRule, IHandle, IHashEnv(..),
    IScalarCwd(..),

    ArrayClass(..), CodeClass(..), HandleClass(..), HashClass(..),
    ObjectClass(..), PairClass(..), RuleClass(..), ScalarClass(..),
    ThunkClass(..),

    CompUnit(..), mkCompUnit, compUnitVersion,

    -- MonadEval(..),

    runEvalSTM, runEvalIO, shiftT, resetT, callCC,
    undef, defined, tryIO, guardSTM, guardIO, guardIOexcept,
    readRef, writeRef, clearRef, dumpRef, forceRef,
    askGlobal, writeVar, readVar,
    readSym, findSym,
    ifListContext, ifValTypeIsa, evalValType, fromVal',
    scalarRef, codeRef, arrayRef, hashRef, thunkRef, pairRef,
    newScalar, newArray, newHash, newHandle, newObject,
    proxyScalar, constScalar, lazyScalar, lazyUndef, constArray,
    retError, retControl, retEmpty, retIVar, readIVar, writeIVar,
    lookupPad, padToList, listToPad,
    mkPrim, mkSub, showRat, showTrueRat,
    cxtOfSigil, typeOfSigil,
    buildParam, defaultArrayParam, defaultHashParam, defaultScalarParam,
    emptyExp,
    isSlurpy, envWant,
    extractPlaceholderVars, fromObject, createObject, createObjectRaw,
    doPair, doHash, doArray,
    unwrap, -- Unwrap(..) -- not used in this file, suitable for factoring out
    newObjectId,
    
    expToEvalVal, -- Hack, should be removed once it's figured out how
) where
import Pugs.Internals
import Pugs.Types
import Pugs.MOP
import Pugs.Cont hiding (shiftT, resetT)
import System.IO.Error (try)
import Data.Typeable
import Data.Generics.Basics (Data(..))
import qualified Data.Set            as Set
import qualified Data.Map            as Map
import qualified Data.Seq            as Seq
import qualified Data.IntMap         as IntMap
import qualified Data.ByteString     as Str
import qualified Data.IntSet         as IntSet
import qualified Data.Generics.Twins as Twins


import Pugs.Parser.Number
import Pugs.AST.Prag
import Pugs.AST.Pos
import Pugs.AST.Scope
import Pugs.AST.SIO
import Pugs.Embed.Perl5

type Str = Str.ByteString
type IntSet = IntSet.IntSet
type SeqOf = Seq.Seq
type ObjectSpace = SeqOf (Weak VObject)

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

errType :: (Typeable a) => a -> String
errType x = show (typeOf x)

createObject :: ObjectId -> [(VStr, Val)] -> Eval VObject
createObject mId attrList = do
    oId    <- newObjectId
    createObjectRaw oId mId [] attrList


createObjectRaw :: (MonadSTM m)
    => ObjectId -> ObjectId -> [(Ident, Val)] -> [(Ident, Val)] -> m VObject
createObjectRaw oId mId slotList attrList = do
    slots   <- liftSTM . newTVar . Map.map lazyScalar $ Map.fromList slotList
    attrs   <- liftSTM . newTVar . Map.map lazyScalar $ Map.fromList attrList
    return $ MkObject
        { objId     = oId
        , objMetaId = mId
        , objSlots  = slots
        , objOpaque = Nothing
        }

newObjectId :: Eval ObjectId
newObjectId = do
    tv <- asks envMaxId
    liftSTM $ do
        rv <- readTVar tv
        writeTVar tv (succ rv)
        return rv
        
fromObject :: forall a. (Typeable a) => VObject -> a
fromObject obj = case objOpaque obj of
    Nothing     -> castFail obj "VObject without opaque"
    Just dyn    -> case fromDynamic dyn of
        Nothing -> castFail obj "VObject's opaque not valueable"
        Just x  -> x

castFailM :: forall a b. (Show a, Typeable b) => a -> String -> Eval b
castFailM v str = fail $ "Cannot cast from " ++ show v ++ " to " ++ errType (undefined :: b) ++ " (" ++ str ++ ")"

castFail :: forall a b. (Show a, Typeable b) => a -> String -> b
castFail v str = error $ "Cannot cast from " ++ show v ++ " to " ++ errType (undefined :: b) ++ " (" ++ str ++ ")"



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

type VScalar = Val

intCast :: Num b => Val -> Eval b
intCast x = fmap fromIntegral (fromVal x :: Eval VInt)

type VList = [Val]
type VSubst = (VRule, Exp)
type VArray = [Val]
type VHash = Map VStr Val
data VThunk = MkThunk
    { thunkExp   :: Eval Val
    , thunkClass :: Class
    }
    deriving (Typeable) {-!derive: YAML_Pos!-}
newtype VProcess = MkProcess (ProcessHandle)
    deriving (Typeable) {-!derive: YAML_Pos!-}

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

-- | Represents a value.
data Val
    = Native VNative
    | Undef  VUndef
    | Pure   VPure
    | Mut    VMut
    | IO     VIO
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl5, JSON!-}

-- | Unboxed or native values. They have their own .ids
data Native
    = UInt !Int      -- 3
    | UNum !Float    -- 4.2
    | UStr !Str      -- 'aloha'
    | UBit !Bool     -- True
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl5, JSON!-}

-- | L<S06/"Undefined types">
data VUndef
    = VUUndef
    | VUFailure    !ObjectId
    | VUWhatever

type VBit  = Bool
type VBool = Bool

data Sign
    = Positive
    | Negative

data VInt 
    = Finite      !Integer
    | Infinite    !Sign
    | NotANumber

data VNum
    = VNRational  !Rational
    = VNFloat     !Float

type VPStr = Str

-- Inf or NaN if either part is Inf or NaN.
data VComplex = MkComplex
    { real      :: !VNum
    , imaginary :: !VNum
    }

data VList = MkList
    { listSeq   :: !VSeq
    , listRange :: !VRange
    }

type VSeq = Seq

data VRange = MkRange
    { rangeFrom :: Val  -- ??
    , rangeTo   :: Val  -- ??
    , rangeNext :: Code -- ??
    }

data VSet = Set Val

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

data VPair = MkPair -- ?? or is this more efficient? data Pair (Val, Val)
    { pairKey :: Val
    , pairVal :: Val
    }

data VMapping = Map Val Val -- XXX what about ordered mappings?

-- | L<S06/"Immutable types">
data VPure
    = VPBit       !VBit
    | VPInt       !VInt
    | VPStr       !VStr
    | VPNum       !VNum
    | VPComplex   !VComplex
    | VPBool      !VBool
    | VPException !VException -- XXX
    | VPCode      !Code
    | VPBlock     !Code -- XXX: or more primitive type?
    | VPList      !VList
    | VPSeq       !VSeq
    | VPRange     !VRange
    | VPSet       !VSet
    | VPJunc      !VJunc
    | VPPair      !VPair
    | VPMapping   !VMapping
    | VSignature  !Sig
    | VCapture    !Capt
    deriving (Show, Eq, Ord, Typeable) {-!derive: YAML_Pos!-}

type Buf = IOUArray Word64 Word8

-- | L<S06/"Mutable types"> minus IO types
--   Computations on these types take place in the STM monad.
data VMut
    = VMScalar    !Scalar
    | VMArray     !Array
    | VMHash      !Hash
    | VMBuf       !Buf
    | VMRoutine   !Routine
    | VMSub       !Routine -- ?
    | VMMethod    !Routine -- ?
    | VMSubmethod !Routine -- ?
    | VMMacro     -- ???
    | VMRegex     !VRule -- XXX: maybe move to pure
    | VMMatch     !VMatch
    | VMPackage   !Package
    | VMModule    !Module
    | VMClass     !Class
    | VMRole      !Role
    | VMGrammar   !Grammar
    | VMObject    !Object  -- ? or ObjectId?
    | VMForeign   ...?
    deriving (Show, Eq, Ord, Typeable) {-!derive: YAML_Pos!-}

-- | Obviously side-effectual types such as file handles.
--   Computations on these types must take place in the IO monad.
data VIO
    = VHandle   !VHandle     -- ^ File handle
    | VSocket   !VSocket     -- ^ Socket handle
    | VThread   !(VThread Val)
    | VProcess  !VProcess    -- ^ PID value
    deriving (Show, Eq, Ord, Typeable) {-!derive: YAML_Pos!-}

instance ScalarClass a where
    doScalarFetch :: a -> Eval Val
    doScalarStore :: a -> Eval Val

scalar_fetch :: Val -> Eval Val
scalar_fetch v@Native{} = return v
scalar_fetch v@Pure{}   = return v
scalar_fetch v@(Mut m)  = case m of
    VMScalar s  -> doScalarFetch s
    VMArray{}   -> return v
    VMObject o  -> callMethod "scalar_fetch" o
    _           -> fail ""
scalar_fetch v@(IO i)   = fail "not implemented: scalar_fetch on IO value"


array_flatten :: Val -> Eval Val
array_flatten v@Native{} = return v
array_flatten v@Pure{} = return v
array_flatten v@(Mut m) = case m of
    VMScalar s  -> array_flatten `withScalar` s
    VMArray  a  -> doArrayFlatten a
    VMObject o  -> callMethod "array_flatten" o
    _           -> fail ""
array_flatten v@(IO i)   = fail "not implemented: array_flatten on IO value"
array_subscript :: Val -> Val -> Eval Val
array_subscript v@Native{} = return v
array_subscript idx v@Pure{} = return v
array_subscript idx v@(Mut m) = case m of
    VMScalar s  -> (array_subscript idx) `withScalar` s
    VMArray  a  -> doArraySubscript idx a
    VMObject o  -> callMethod "array_subscript" o [idx]
    _           -> fail ""
array_subscript v@(IO i)   = fail "not implemented: array_subscript on IO value"


hash_flatten :: Val -> Eval Val
hash_flatten v@Native{} = return v
hash_flatten v@Pure{} = return v
hash_flatten v@(Mut m) = case m of
    VMScalar s  -> hash_flatten `withScalar` s
    VMHash   a  -> doHashFlatten a
    VMObject o  -> callMethod "hash_flatten" o
    _           -> fail ""
hash_flatten v@(IO i)   = fail "not implemented: hash_flatten on IO value"
hash_subscript :: Val -> Val -> Eval Val
hash_subscript v@Native{} = return v
hash_subscript idx v@Pure{} = return v
hash_subscript idx v@(Mut m) = case m of
    VMScalar s -> (hash_subscript idx) `withScalar` s
    VMHash   a -> doHashSubscript idx a
    VMObject o -> callMethod "hash_subscript" o [idx]
    _          -> fail ""
hash_subscript v@(IO i)   = fail "not implemented: hash_subscript on IO value"


instance ArrayClass (TVar Val) where
    doArrayFlatten


instance ScalarClass (TVar Val) where
    -- vivify?
    doScalarFetch t = do
        lv <- asks envLValue
        case lv of
            RValue -> liftSTM (readTVar t)
            LValue typ -> do
                rv <- readTVar t
                case rv of
                    Pure VPUndef -> do
                        writeTVar t =<< fmap Mut (newLValue typ)
                    _ -> return rv


{-|
Find the 'Type' of the value contained by a 'Val'.

See "Pugs.Types" for info on types.
-}
valClass :: Val -> Class
valClass VUndef          = mkClass "Scalar"
valClass (VBool    _)    = mkClass "Bool"
valClass (VInt     _)    = mkClass "Int"
valClass (VRat     _)    = mkClass "Rat"
valClass (VNum     _)    = mkClass "Num"
valClass (VComplex _)    = mkClass "Complex"
valClass (VStr     _)    = mkClass "Str"
-- valClass (VList    _)    = mkClass "List"
valClass (VList    _)    = mkClass "Array"
valClass (VCode    c)    = code_iClass c
valClass (VBlock   _)    = mkClass "Block"
valClass (VJunc    _)    = mkClass "Junction"
valClass (VError _ _)    = mkClass "Error"
valClass (VHandle  _)    = mkClass "IO"
valClass (VSocket  _)    = mkClass "Socket"
valClass (VThread  _)    = mkClass "Thread"
valClass (VProcess _)    = mkClass "Process"
valClass (VControl _)    = mkClass "Control"
valClass (VRule    _)    = mkClass "Pugs::Internals::VRule"
valClass (VSubst   _)    = mkClass "Subst"
valClass (VMatch   _)    = mkClass "Match"
valClass (VClass    t)    = t
valClass (VObject  o)    = objClass o
valClass (VOpaque  _)    = mkClass "Object"
valClass (PerlSV   _)    = mkClass "Scalar::Perl5"

type VBlock = Exp
data VControl
    = ControlExit  !ExitCode
    | ControlEnv   !Env
-- \| ControlLeave !(Env -> Eval Bool) !Val
    deriving (Show, Eq, Ord, Typeable) -- don't derive YAML for now


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

--xxx gaal
--isSlurpy :: Param -> Bool
--isSlurpy param = isSlurpyCxt $ paramCtx param

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

type Ident = Str -- XXX wrong

-- | AST for a statement. The top level of an AST is a list of Stmt.
data Stmt = MkStmt
    { label      :: Maybe Ident
    , pragmas    :: Map Ident Val
    , expression :: Exp
    } deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl5, JSON!-}

-- | Carry over last pragmas and create a new statement out of an expression
nextStmt :: Stmt -> Exp -> Stmt
nextStmt MkStmt{ pragmas=prag } exp = MkStmt{ label=Nothing, pragmas=prag, expression=exp }

-- | AST for an expression.
data Exp
    = Noop                               -- ^ No-op
    | ExpVar      Var                    -- ^ Variable
    | ExpVal      Val                    -- ^ Value
    | ExpDeref    Var                    -- ^ Dereference
    | ExpBind     Exp  Exp               -- ^ Bind, i.e., :=
    | ExpAssign   Exp  Exp               -- ^ Assignment, =
    | ExpControl  Cont                   -- ^ Control structure, e.g. if, while
    | ExpFlatten  [Exp]                  -- ^ Wrapper for expressions forced into
                                         --   slurpy context
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl5, JSON!-}
    -- ???| Prim !([Val] -> Eval Val)         -- ^ Primitive


-- | Control statement, such as "if".
data Control
    = ContCall        Exp  Capt            -- ^ lookup a routine, call it
    | ContApply       Code Capt            -- ^ apply a Code immediately
    | ContCond        Exp  Code            -- ^ 2 if 1
    | ContTrenaryCond Exp  Code  Code      -- ^ 1 ?? 2 !! 3
    | ContCondBlock   (Exp, Code) [(Exp, Code)] (Maybe Code)
                                           -- ^ if 1 { 2 } else { 3 } or in general,
                                           --   if 1 { 2 } elsif 3 { 4 } elsif 5 { 6 } 7
                                           -- ^ &statement_control:<if>
    | ContGoto        Ident                -- ^ &statement_control:<goto>
    | ContWhile       Exp  Code            -- ^ &statement_control:<while>
    | ContGiven       Exp  Code            -- ^ given
    | ContWhen        Exp  Code            -- ^ when
    | ContForeign                          -- ^ &statement_control:<mycontrol>
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl5, JSON!-}

-- | Single parameter for a function/method, e.g.:
--   Elk $m where { $m.antlers ~~ Velvet }
{-|
A formal parameter of a sub (or other callable).

These represent declared parameters; don't confuse them with actual argument
values.
-}
data Param = MkParam
    { paramVariable    :: Ident         -- ^ E.g. $m above
    , paramTypes       :: [Type]        -- ^ Static pieces of inferencer-food
                                        --   E.g. Elk above
    , paramConstraints :: [Code]        -- ^ Dynamic pieces of runtime-mood
                                        --   E.g. where {...} above
    , paramUnpacking   :: Maybe Sig     -- ^ E.g. BinTree $t (Left $l, Right $r)
    , paramDefault     :: Maybe Exp     -- ^ E.g. $answer? = 42
    , paramLabel       :: Ident         -- ^ E.g. :mode
    , paramSlots       :: Map Ident Val -- ^ Any additional attrib not
                                        --   explicitly mentioned below
    , paramHasAccess   :: ParamAccess   -- ^ is ro, is rw, is copy
    , paramIsRef       :: Bool
    , paramIsLazy      :: Bool
    } deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl5, JSON!-}

data ParamAccess
    = ParamAccessRO
    | ParamAccessRW
    | ParamAccessCopy
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl5, JSON!-}

-- | Function associtivity
data Assoc
    = AssocLeft
    | AssocRight
    | AssocNon
    | AssocChain
    | AssocList
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl5, JSON!-}

-- | AST for function signature. Separated to method and function variants
--   for ease of pattern matching.
data Sig
    = MkSigMethSingle
        { sigInvocant               :: Param
        , requiredPositionalCount   :: Int
        , requiredNames             :: Set Ident
        , positionalList            :: [Param]
        , namedSet                  :: Map Ident Param
        , slurpyScalarList          :: [Param]
        , slurpyArray               :: Maybe Param
        , slurpyHash                :: Maybe Param
        , slurpyCode                :: Maybe Param
        , slurpyCapture             :: Maybe Param
        }
    | MkSigSubSingle
        { requiredPositionalCount   :: Int
        , requiredNames             :: Set Ident
        , positionalList            :: [Param]
        , namedSet                  :: Map Ident Param
        , slurpyScalarList          :: [Param]
        , slurpyArray               :: Maybe Param
        , slurpyHash                :: Maybe Param
        , slurpyCode                :: Maybe Param
        , slurpyCapture             :: Maybe Param
        }
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl5, JSON!-}


newtype CodeWrapping
    = MkCodeWrapping
        { codeWrappings :: TVar IntMap Routine
        }
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl5, JSON!-}

data Routine
    = MkSingleRoutine
        { wrappings       :: CodeWrapping
        , routineCode     :: Code
        }
    | MkMultiRoutine
        { wrappings       :: CodeWrapping
        , routineVariants :: Set MultiVariant
        }
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl5, JSON!-}

{- (It's not clear how multi variants are supposed to be ordered.
   One thing is that we want local code to be able to add variants,
   but we also presumably want to express guards with multimethods.
-}

-- | AST for a primitive Code object
data Code
    = MkCode
        { signature    :: Sig
        , body         :: [Stmt]                -- ^ AST of "do" block
        , pad          :: Pad                   -- ^ Storage for lexical vars
        , codeTraits   :: Map Ident Val         -- ^ Any additional trait not
                                                --   explicitly mentioned below
        , codeIsRW     :: Bool
        , codeIsCached :: Bool
        , codeIsSafe   :: Bool
        , precedence   :: Rational
        , assoc        :: Assoc
        , preBlocks    :: [Code]
        , postBlocks   :: [Code]
        , firstBlocks  :: [Code]
        , lastBlocks   :: [Code]
        , nextBlocks   :: [Code]
        , keepBlocks   :: [Code]
        , undoBlocks   :: [Code]
        }
    | MkPrim
        { signature    :: Sig
        , precedence   :: Rational
        , assoc        :: Assoc
        , codeIsRW     :: Bool
        , codeIsSafe   :: Bool
        }
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl5, JSON!-}
    
data MultiVariant = MkMultiVariant 
    { semicolonOffsets              :: IntSet
    , callable                      :: Code  -- ^ Thing actually called
    , extraWrappinhs                :: Maybe CodeWrapping
    } deriving (Show, Eq, Ord, Data, Ty) {-!derive: YAML_Pos, Perl5, JSON!-}

-- | Storage cell for a lexical variabl
data PadEntry = MkPadEntry
    { cellType :: StorageType    -- ^ m etc.
    , cellVar  :: TVar Val       -- ^ stored value
    }
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl5, JSON!-}

type Bit = Bool     -- XXX: correct?

type ObjectId = Native

type MMap = TVar (Map Ident Val)

data Object
    = MkObject
        { objId       :: !ObjectId           -- ^ our unique id
        , objMeta     :: !Class              -- ^ id of our metaobj/type
        , objSlots    :: !MMap               -- ^ storage for explicit fields
        }
    | MkForeign
        { objId       :: !ObjectId           -- ^ our unique id
        , objMeta     :: !Class              -- ^ id of our metaobj/type
        , objOpaque   :: !Dynamic            -- ^ storage for opaque wrapped obj
        }
    | MkPrototype
        { objId       :: !ObjectId           -- ^ our unique id
        , objMeta     :: !Class              -- ^ id of our metaobj/type
        }
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl5, JSON!-}

data Capt
    = MkCaptMeth
        { captInvocant :: Exp
        , argstack     :: [Arglist]
        }
    | MkCaptSub
        { argstack     :: [Arglist]
        }
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl5, JSON!-}

data Arglist = MkArglist
    { positional :: [Exp]
    , named      :: Map Str [Exp]
    }
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl5, JSON!-}

data Var
    = VarLexical
        { varName     :: Ident
        , callerCount :: Int
        , outerCount  :: Int
        }
    | VarDynamic
        { varName     :: Ident
        , packageName :: [Ident]
        }
    | VarMagic Magic
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl5, JSON!-}

data Magic
    = MagicOS               -- ^ $?OS        Which os am I compiled for?
    | MagicOSVer            -- ^ $?OSVER     Which os version am I compiled for?
    | MagicPerlVer          -- ^ $?PERLVER   Which Perl version am I compiled for?
    | MagicFile             -- ^ $?FILE      Which file am I in?
    | MagicLine             -- ^ $?LINE      Which line am I at?
    | MagicScalarPackage    -- ^ $?PACKAGE   Which package am I in?
    | MagicArrayPackages    -- ^ @?PACKAGE   Which packages am I in?
    | MagicScalarModule     -- ^ $?MODULE    Which module am I in?
    | MagicArrayModules     -- ^ @?MODULE    Which modules am I in?
    | MagicScalarClass      -- ^ $?CLASS     Which class am I in? (as variable)
    | MagicArrayClasses     -- ^ @?CLASS     Which classes am I in?
    | MagicScalarRole       -- ^ $?ROLE      Which role am I in? (as variable)
    | MagicArrayRoles       -- ^ @?ROLE      Which roles am I in?
    | MagicScalarGrammar    -- ^ $?GRAMMAR   Which grammar am I in?
    | MagicArrayGrammars    -- ^ @?GRAMMAR   Which grammars am I in?
    | MagicParser           -- ^ $?PARSER    Which Perl grammar was used to
                            -- ^                   parse this statement?
    | MagicScalarRoutine    -- ^ &?ROUTINE   Which routine am I in?
    | MagicArrayRoutines    -- ^ @?ROUTINE   Which routines am I in?
    | MagicScalarBlock      -- ^ &?BLOCK     Which block am I in?
    | MagicArrayBlocks      -- ^ @?BLOCK     Which blocks am I in?
    deriving (Show, Eq, Ord, Data, Typeable) {-!derive: YAML_Pos, Perl5, JSON!-}

data StorageType -- XXX: is this the same as today's data Scope?


{- FIXME: Figure out how to get this working without a monad, and make it castV -}
expToEvalVal :: Exp -> Eval Val
expToEvalVal exp = do
    obj <- createObject (mkType "Code::Exp") []
    return $ VObject obj{ objOpaque = Just $ toDyn exp }



{- presumably no longer needed now that annotations aren't wrapping nodes --gaal
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
-}

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

{-| Deduce the placeholder vars ($^a, $^x etc.) used by a block). -}
extractPlaceholderVars :: Data d => d -> Set String -> (d, a)
extractPlaceholderVars ast accum = Twins.gmapAccumT doExtractPlaceholderVars accum ast

doExtractPlaceholderVars :: Data d => a -> d -> (a, d)
doExtractPlaceholderVars vs (ExpVar name)
    | (sigil:'^':identifer) <- name
    , name' <- (sigil : identifer)
    = (insert $ name vs, ExpVar name')
    | name == "$_"
    = (insert $ name vs, ExpVar name)
    | otherwise
    = (vs, ExpVar name)
doExtractPlaceholderVars vs (ContGiven exp code) = (vs'', ContGiven exp code)
    where
    (vs', _) = extractPlaceholderVars exp (delete $ "$_" vs)
    (vs'', _) = extractPlaceholderVars code vs'
doExtractPlaceholderVars vs (ContWhen exp code) = (vs'', ContWhen exp code)
    where
    (vs', _) = extractPlaceholderVars exp (insert $ "$_" vs)
    (vs'', _) = extractPlaceholderVars code vs'

buildParam :: String -- ^ Type of the parameter
           -> String -- ^ Parameter-sigil (@:@, @!:@, @?@, @!@, etc.)
           -> String -- ^ Name of the parameter (including primary sigil)
           -> Exp    -- ^ Expression for the param's default value
           -> Param
buildParam name def = MkParam
    { paramHasAccess  = ParamAccessRW -- caller should override
    , paramIsLazy     = False
    , paramVariable   = name
    , paramDefault    = def
    }

defaultCaptureSig, defaultArrayParam, defaultHashParam, defaultScalarParam :: Param
defaultCaptureSig  = buildParam "$_" Nothing
defaultArrayParam  = buildParam "@_" (Just $ ExpVal VUndef)
defaultHashParam   = buildParam "%_" (Just $ ExpVal VUndef)
defaultScalarParam = buildParam "$_" (Just $ ExpVar "$_")

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
    , envMaxId   :: !(TVar ObjectId)     -- ^ Current max object id
    , envAtomic  :: !Bool                -- ^ Are we in an atomic transaction?
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
'Pugs.Eval.reduce' entry for ('Pad' 'SMy' ... ).

The current global and lexical pads are stored in the current 'Env', which
is stored in the @Reader@-monad component of the current 'Eval' monad.
-}

newtype Pad = MkPad { padEntries :: Map Var PadEntry }
    deriving (Eq, Ord, Show, Typeable)

data IHashEnv = MkHashEnv deriving (Show, Typeable) {-!derive: YAML_Pos!-}
data IScalarCwd = MkScalarCwd deriving (Show, Typeable) {-!derive: YAML_Pos!-}

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

-- | Look up a symbol in a 'Pad', returning the ref it is bound to.
lookupPad :: Var -- ^ Symbol to look for
          -> Pad -- ^ Pad to look in
          -> Maybe (TVar PadEntry) -- ^ Might return 'Nothing' if var is not found

{-
    We (may) have to fix the name, as the user can write things like
        &::("infix:<+>")(2, 3)
    which, without fixName, wouldn't work, as all operators are currently
    stored as &infix:+, i.e. without the brackets.
-}

lookupPad key (MkPad map) = Map.lookup (possiblyFixOperatorName key) map

{-|
Transform a pad into a flat list of bindings. The inverse of 'mkPad'.

Note that @Data.Map.assocs@ returns a list of mappings in ascending key order.
-}
padToList :: Pad -> [(Var, PadEntry)]
padToList (MkPad map) = Map.assocs map

listToPad :: [(Var, PadEntry)] -> Pad
listToPad = MkPad . Map.fromList

-- | type for a function introducing a change to a Pad
type PadMutator = (Pad -> Pad)

{-|
Serializable compilation unit

See: docs/notes/precompilation_cache.pod
-}
data CompUnit = MkCompUnit
    { ver  :: Int        -- a version number, currently 1
    --, desc :: String     -- e.g., the name of the contained module
    , ast  :: Exp        -- AST of unit
    } deriving (Show, Eq, Ord, Typeable) {-!derive: YAML_Pos !-}

mkCompUnit :: String -> Exp -> CompUnit
mkCompUnit _ pad ast = MkCompUnit compUnitVersion ast

compUnitVersion :: Int
compUnitVersion = 4

{- Eval Monad -}
-- type Eval x = EvalT (ContT Val (ReaderT Env SIO)) x
type Eval = EvalT (ContT Val (ReaderT Env SIO))
newtype EvalT m a = EvalT { runEvalT :: m a }

runEvalSTM :: Env -> Eval Val -> STM Val
runEvalSTM env = runSTM . (`runReaderT` env { envAtomic = True }) . (`runContT` return) . runEvalT

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

guardSTM :: STM a -> Eval a
guardSTM stm = do
    rv <- liftSTM $ fmap Right stm `catchSTM` (return . Left)
    case rv of
        Left e -> fail (show e)
        Right v -> return v
    
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

If t
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
    -- liftSTM stm = EvalT (lift . lift . liftSTM $ stm)
    liftSTM stm = do
        atom <- asks envAtomic
        if atom
            then EvalT (lift . lift . liftSTM $ stm)
            else EvalT (lift . lift . liftIO . liftSTM $ stm)

instance MonadReader Env Eval where
    ask       = lift ask
    local f m = EvalT $ local f (runEvalT m)

-- XXX: transition: renamed from findSymRef
readSym :: (MonadSTM m) => String -> Pad -> m Val
readSym name pad = do
    case findSym name pad of
        Just tv -> liftSTM $ readTVar tv
        Nothing -> fail $ "Cannot find variable: " ++ show name

findSym :: String -> Pad -> Maybe (TVar PadEntry)
findSym name pad = case lookupPad name pad of
    Just (x:_)  -> Just x
    _           -> Nothing

instance MonadCont Eval where
    -- callCC :: ((a -> Eval b) -> Eval a) -> Eval a
    callCC f = EvalT . callCCT $ \c -> runEvalT . f $ \a -> EvalT $ c a

{-
instance MonadEval Eval

class (MonadReader Env m, MonadCont m, MonadIO m, MonadSTM m) => MonadEval m
--     askGlobal :: m Pad
-}

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
defined VType{} = False
defined _       = True
-- | Produce an undefined Perl 6 value (i.e. 'VUndef').
undef :: VScalar
undef = VUndef

{- transition: do we need a Val -> Val pure form?
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
-}

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

newObject :: (MonadSTM m) => Type -> m VRef
newObject typ = case showType typ of
    "Item"      -> liftSTM $ fmap scalarRef $ newTVar undef
    "Scalar"    -> liftSTM $ fmap scalarRef $ newTVar undef
    "Array"     -> liftSTM $ fmap arrayRef $ (newTVar IntMap.empty :: STM IArray)
    "Hash"      -> liftSTM $ fmap hashRef $ (newTVar Map.empty :: STM IHash)
    "Code"      -> liftSTM $ fmap codeRef $ newTVar mkPrim
        { subAssoc = ""
        , subBody  = MkPrim $ const $ fail "Cannot use Undef as a Code object"
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
            ref@(MkRef (IHash hv)) <- newObject (mkType "Hash")
            scalar_store sv (VRef ref)
            return $ f hv
        _  -> doHash val f
doHash (VRef (MkRef p@(IPair _))) f = return $ f p
doHash (VObject o) f = return $ f (objSlots o)
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
            ref@(MkRef (IArray hv)) <- newObject (mkType "Array")
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

-- | An empty failed match
mkMatchFail :: VMatch
mkMatchFail = MkMatch False 0 0 "" [] Map.empty

-- | Makes a successful match
mkMatchOk :: Int -> Int -> VStr -> VList -> VHash -> VMatch
mkMatchOk   = MkMatch True

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
    maxi <- newTVar 1
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

instance YAML ([Val] -> Eval Val) where
    asYAML _ = return nilNode
    fromYAML _ = return (const $ return VUndef)
instance YAML ObjectId where
    asYAML (MkObjectId x) = asYAML x
    fromYAML x = fmap MkObjectId (fromYAML x)
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
        fail ("Not implemented: asYAML \"" ++ showType (refType ref) ++ "\"")
    fromYAML MkYamlNode{nodeTag=Just s, nodeElem=YamlSeq [node]}
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
instance Typeable Regex where typeOf _ = typeOf ()
#if __GLASGOW_HASKELL__ <= 604
instance Typeable1 Tree where typeOf1 _ = typeOf ()
#endif


{- !!! For DrIFT -- Don't delete !!!

data Scope = SState | SLet | STemp | SEnv | SMy | SOur | SGlobal
    {-!derive: YAML_Pos, JSON, Perl5!-}

newtype Pad = MkPad { padEntries :: Map Var PadEntry }
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
