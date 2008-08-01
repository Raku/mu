{-# OPTIONS_GHC -cpp -fglasgow-exts -fno-warn-orphans -fallow-overlapping-instances -fallow-undecidable-instances -fparr #-}

module Pugs.AST.Internals (
    Eval(..),      -- uses Val, Env, SIO
    Ann(..),   -- Cxt, Pos, Prag
    Exp(..),   -- uses Pad, Eval, Val
    Env(..),   -- uses Pad, TVar, Exp, Eval, Val
    Val(..),   -- uses V.* (which ones?)
    Value(..), -- uses Val, Eval
    InitDat(..),
    SubAssoc(..), TraitBlocks(..), emptyTraitBlocks,

    MPad(..), LexPad(..), LexPads, Pad(..), PadEntry(..), EntryFlags(..), PadMutator, -- uses Var, TVar, VRef
    Param(..), -- uses Cxt, Exp
    Params, -- uses Param
    Bindings, -- uses Param, Exp
    SlurpLimit, -- VInt, Exp
    
    emptyPad,

    VRef(..), -- uses IVar
    VOpaque(..), -- uses Value
    VControl(..), -- uses Env, Eval, Val
    ControlLoop(..), ControlWhen(..), Frame(..),
    VScalar, -- uses Val
    VPair, -- uses Val
    VList, -- uses Val
    VThread(..), -- uses Val
    VSubst(..),  -- uses VRule, VStr, Exp
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
    VMultiCode(..),

    IVar(..), -- uses *Class and V*
    IArray(..), IArraySlice, IHash, IScalar, IScalarProxy,
    IScalarLazy, IPairHashSlice, IRule, IHandle, IHashEnv(..),
    IScalarCwd(..),

    ArrayClass(..), CodeClass(..), HandleClass(..), HashClass(..),
    ObjectClass(..), PairClass(..), RuleClass(..), ScalarClass(..),
    ThunkClass(..),

    CompUnit(..), mkCompUnit, compUnitVersion,

    -- MonadEval(..),

    transformExp,

    runEvalSTM, runEvalIO, callCC, tryT, resetT, shiftT, catchT,
    undef, defined, tryIO, guardSTM, guardIO, guardIOexcept,
    readRef, writeRef, clearRef, dumpRef, forceRef,
    askGlobal, writeVar, readVar,
    findSymRef, findSym, valType,
    ifListContext, ifValTypeIsa, evalValType, fromVal',
    scalarRef, codeRef, arrayRef, hashRef, thunkRef, pairRef,
    newScalar, newArray, newHash, newHandle, newObject,

    cloneRef, cloneIVar,

    proxyScalar, constScalar, lazyScalar, lazyUndef, constArray,
    retControl, retShift, retShiftEmpty, retEmpty, retIVar, readIVar, writeIVar,
    fromVals, refType,
    readPadEntry, writePadEntry, refreshPad, lookupPad, padToList, listToPad,
    mkPrim, mkSub, mkCode, showRat, showTrueRat,
    cxtOfSigil, cxtOfSigilVar, typeOfSigil, typeOfSigilVar,
    buildParam, defaultArrayParam, defaultHashParam, defaultScalarParam,
    paramsToSig,
    emptyExp,
    isSlurpy, envWant,
    extractPlaceholderVars, fromObject, createObject, createObjectRaw,
    doPair, doHash, doArray,
    unwrap,
    newObjectId, runInvokePerl5,
    
    showVal, errStr, errStrPos, errValPos, enterAtomicEnv, valToBool, envPos', -- for circularity
    expToEvalVal, -- Hack, should be removed once it's figured out how

    newSVval, -- used in Run.Perl5

    anyToVal, vvToVal, anyFromVal, -- for circularity

    DebugInfo, newDebugInfo, _Sym, _Var -- String -> ByteString constructors
) where

import Pugs.Internals
import Pugs.Types
import qualified Data.Set       as Set
import qualified Data.Map       as Map

import qualified Data.HashTable    as H
import GHC.Conc (unsafeIOToSTM)

import Pugs.Cont (callCC)
import Pugs.Parser.Number
import Pugs.AST.Types
import Pugs.AST.Functions
import Pugs.AST.Eval
import Pugs.AST.Utils
import Pugs.AST.Prag
import Pugs.AST.Pos
import Pugs.AST.Scope
import Pugs.AST.SIO
import Pugs.Embed.Perl5
import qualified Pugs.Val as Val
import GHC.PArr
import {-# SOURCE #-} Pugs.AST

-- CPP Includes

#include "../Types/Array.hs"
#include "../Types/Handle.hs"
#include "../Types/Hash.hs"
#include "../Types/Scalar.hs"
#include "../Types/Code.hs"
#include "../Types/Thunk.hs"
#include "../Types/Rule.hs"
#include "../Types/Pair.hs"
#include "../Types/Object.hs"

-- Data Definitions

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
    | VThread   !VThread
    | VProcess  !VProcess    -- ^ PID value
    | VRule     !VRule       -- ^ Rule\/regex value
    | VSubst    !VSubst      -- ^ Substitution value (correct?)
    | VMatch    !VMatch      -- ^ Match value
    | VObject   !VObject     -- ^ Object
    | VOpaque   !VOpaque
    | PerlSV    !PerlSV
    | VV        !Val.Val
    deriving (Show, Eq, Ord, Typeable)

{-|
Evaluation environment.

The current environment is stored in the @Reader@ monad inside the current 
'Eval' monad, and can be retrieved using @ask@ for the whole 'Env', or @asks@ 
if you just want a single field.
-}
data Env = MkEnv
    { envContext :: !Cxt                -- ^ Current context
                                        -- ('CxtVoid', 'CxtItem' or 'CxtSlurpy')
    , envLValue  :: !Bool               -- ^ Are we in an LValue context?
    , envLexical :: !Pad                -- ^ Cached lexical pad for variable lookup
    , envLexPads :: !LexPads            -- ^ Current lexical pads; MY is leftmost, OUTER is next, etc
    , envCaller  :: !(Maybe Env)        -- ^ CALLER pads
    , envCompPad :: !(Maybe MPad)       -- ^ Current COMPILING pad
    , envGlobal  :: !MPad               -- ^ Global pad for variable lookup
    , envPackage :: !Pkg                -- ^ Current package
    , envEval    :: !(Exp -> Eval Val)  -- ^ Active evaluator
    , envBody    :: !Exp                -- ^ Current AST expression
    , envFrames  :: !(Set Frame)        -- ^ Special-markers in the dynamic path
    , envDebug   :: !DebugInfo          -- ^ Debug info map
    , envPos     :: !Pos                -- ^ Source position range
    , envPragmas :: ![Pragma]           -- ^ List of pragmas in effect
    , envInitDat :: !(TVar InitDat)     -- ^ BEGIN result information
    , envMaxId   :: !(TVar ObjectId)    -- ^ Current max object id
    , envAtomic  :: !Bool               -- ^ Are we in an atomic transaction?
    } 
    deriving (Show, Eq, Ord, Typeable) -- don't derive YAML for now

data IVar v where
    IScalar :: ScalarClass a => !a -> IVar VScalar
    IArray  :: ArrayClass  a => !a -> IVar VArray
    IHash   :: HashClass   a => !a -> IVar VHash
    ICode   :: CodeClass   a => !a -> IVar VCode
    IHandle :: HandleClass a => !a -> IVar VHandle
    IRule   :: RuleClass   a => !a -> IVar VRule
    IThunk  :: ThunkClass  a => !a -> IVar VThunk
    IPair   :: PairClass   a => !a -> IVar VPair
    IVal    ::                !Val -> IVar Val

data VOpaque where
    MkOpaque :: Value a => !a -> VOpaque

-- GADTs, here we come!
data VRef where
    MkRef   :: (Typeable a) => !(IVar a) -> VRef

data VObject = MkObject
    { objType   :: !VType
    , objAttrs  :: !IHash
    , objOpaque :: !(Maybe Dynamic)
    , objId     :: !ObjectId
    }
    deriving (Show, Eq, Ord, Typeable) {-!derive: YAML_Pos!-}

-- | Represents an expression tree.
data Exp
    = Noop                              -- ^ No-op
    | App !Exp !(Maybe Exp) ![Exp]      -- ^ Function application
                                        --     e.g. myfun($invocant: $arg)
    | Syn !String ![Exp]                -- ^ Syntactic construct that cannot
                                        --     be represented by 'App'.
    | Ann !Ann !Exp                     -- ^ Annotation (see @Ann@)
--  | Pad !Scope !Pad !Exp              -- ^ Lexical pad
    | Sym !Scope !Var !EntryFlags !Exp !Exp -- ^ Symbol declaration
    | Stmts !Exp !Exp                   -- ^ Multiple statements
    | Prim !([Val] -> Eval Val)         -- ^ Primitive
    | Val !Val                          -- ^ Value
    | Var !Var                          -- ^ Variable
    | NonTerm !Pos                      -- ^ Parse error
    deriving (Show, Eq, Ord, Typeable) {-!derive: YAML_Pos!-}

newtype ObjectId = MkObjectId { unObjectId :: Int }
    deriving (Show, Eq, Ord, Typeable) {-!derive: YAML_Pos!-}

-- Type Synonyms

type VType = Type
type VArray = [Val]
type VHash = Map VStr Val
type VList = [Val]

-- Functions

{-|
Return the appropriate 'empty' value for the current context -- either
an empty list ('VList' []), or undef ('VUndef').
-}
retEmpty :: Eval Val
retEmpty = do
    ifListContext
        (return $ VList [])
        (return VUndef)

retShiftEmpty :: Eval a
retShiftEmpty = retShift =<< retEmpty

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
    vt  <- evalValType v
    if isaType typ vt
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
    v <- io $ svToVal sv
    case v of
        PerlSV sv'  -> fromSV sv'   -- it was a SV
        VV vv
            | Just sv  <- Val.castVal vv -> fromSV sv
            | Just v   <- Val.castVal vv -> fromVal v
        val         -> fromVal val  -- it was a Val
fromVal' (VV vv) = do
    v' <- vvToVal vv
    case v' of
        VV vv''     -> fromVV vv''
        PerlSV sv   -> fromSV sv
        _           -> fromVal v'
fromVal' v = doCast v

-- XXX - This is makeshift until all our native types are in VV.
vvToVal :: Val.Val -> Eval Val
vvToVal x
    | Just sv <- Val.castVal x  = do
        rv <- io (svToVal sv)
        case rv of
            VV vv
                | Just sv  <- Val.castVal vv -> return (PerlSV sv)
                | Just v   <- Val.castVal vv -> return v
            _ -> return rv
    | Just v  <- Val.castVal x  = return v
    | Just x' <- Val.castVal x  = return . VStr $ (cast :: Val.PureStr -> String)  x'
    | Just x' <- Val.castVal x  = return . VInt $ (cast :: Val.PureInt -> Integer) x'
    | Just x' <- Val.castVal x  = return . VNum $ (cast :: Val.PureNum -> Double)  x'
    | Just x' <- Val.castVal x  = return (VStr x')
    | Just x' <- Val.castVal x  = return (VInt x')
    | Just x' <- Val.castVal x  = return (VNum x')
    | Just x' <- Val.castVal x  = return (VBool x')
    | Just () <- Val.castVal x  = return VUndef
    | otherwise                 = return (VV x)

getArrayIndex :: Int -> Maybe (IVar VScalar) -> Eval IArray -> Maybe (Eval b) -> Eval (IVar VScalar)
getArrayIndex idx def getArr _ | idx < 0 = do
    -- first, check if the list is at least abs(idx) long
    MkIArray iv <- getArr
    a   <- stm $ readTVar iv
    let size = a_size a
    if size > abs (idx+1)
        then return (a !: (idx `mod` size))
        else errIndex def idx
-- now we are all positive; either extend or return
getArrayIndex idx def getArr ext = do
    MkIArray iv <- getArr
    a   <- stm $ readTVar iv
    let size = a_size a
    if size > idx
        then return (a !: idx)
        else case ext of
            Just doExt -> do { doExt; getArrayIndex idx def getArr Nothing }
            Nothing    -> errIndex def idx

createObjectRaw :: (MonadSTM m)
    => ObjectId -> Maybe Dynamic -> VType -> [(VStr, Val)] -> m VObject
createObjectRaw uniq opaq typ attrList = do
    attrs   <- stm . unsafeIOToSTM . H.fromList H.hashString $ map (\(a,b) -> (a, lazyScalar b)) attrList
    return $ MkObject
        { objType   = typ
        , objId     = uniq
        , objAttrs  = attrs
        , objOpaque = opaq
        }


runInvokePerl5 :: PerlSV -> PerlSV -> [PerlSV] -> Eval Val
runInvokePerl5 sub inv args = do 
    env     <- ask
    rv      <- io $ do
        envSV   <- mkEnv env
        invokePerl5 sub inv args envSV (enumCxt $ envContext env)
    case rv of
        Perl5ReturnValues [x]   -> io $ svToVal x
        Perl5ReturnValues xs    -> io $ fmap VList (mapM svToVal xs)
        Perl5ErrorString str    -> fail str
        Perl5ErrorObject err    -> throwError (PerlSV err)

anyToVal :: (Show a, Typeable a) => a -> Val
anyToVal x
    | Just v <- fromTypeable x      = v
    | Just v <- fromTypeable x      = PerlSV v
    | Just v <- fromTypeable x      = VStr v
    | Just v <- fromTypeable x      = VInt v
    | Just v <- fromTypeable x      = VNum v
    | Just () <- fromTypeable x     = VUndef
    | otherwise                     = error (show x)

newSVval :: Val -> IO PerlSV
newSVval val = case val of
    PerlSV sv   -> return sv
    VStr str    -> vstrToSV str
    VType typ   -> vstrToSV (showType typ)
    VBool bool  -> vintToSV (fromEnum bool)
    VInt int    -> vintToSV int
    VRat rat    -> vnumToSV rat
    VNum num    -> vnumToSV num
    VRef ref    -> vrefToSV ref
    VCode{}     -> mkValRef val "Code"
    VBlock{}    -> mkValRef val "Code"
    VHandle{}   -> mkValRef val "Handle"
    VSocket{}   -> mkValRef val "Socket"
    VList{}     -> mkValRef val "Array"
    VUndef      -> svUndef
    VError{}    -> svUndef
    _           -> mkValRef val ""

vrefToSV :: VRef -> IO PerlSV
vrefToSV ref = mkValRef (VRef ref) $ case ref of
    MkRef IScalar{}   -> "Scalar"
    MkRef IArray{}    -> "Array"
    MkRef IHash{}     -> "Hash"
    MkRef ICode{}     -> "Code"
    MkRef IHandle{}   -> "Handle"
    MkRef IRule{}     -> "Rule"
    MkRef IThunk{}    -> "Thunk"
    MkRef IPair{}     -> "Pair"
    MkRef (IVal v)    -> show (valType v)

valToStr :: Val -> Eval VStr
valToStr = fromVal


errStr :: VStr -> Val
errStr str = VError (VStr str) []

errStrPos :: VStr -> Pos -> Val
errStrPos str pos = VError (VStr str) [pos]

errValPos :: Val -> Pos -> Val
errValPos val pos = VError val [pos]

enterAtomicEnv :: Env -> Env
enterAtomicEnv env = env{ envAtomic = True }

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
valType (VRule    _)    = mkType "Regex"
valType (VSubst   _)    = mkType "Subst"
valType (VMatch   _)    = mkType "Match"
valType (VType    t)    = t
valType (VObject  o)    = objType o
valType (VOpaque  _)    = mkType "Object"
valType (PerlSV   _)    = mkType "Scalar::Perl5"
valType (VV       _)    = mkType "Scalar::Perl5" -- (cast $ Val.valMeta v)

valToBool :: Val -> Eval VBool
valToBool = fromVal

_Sym :: Scope -> String -> EntryFlags -> Exp -> Exp -> Exp
_Sym scope str flags init rest = Sym scope (cast str) flags init rest

_Var :: String -> Exp
_Var str = Var (possiblyFixOperatorName (cast str))

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
-- transformExp f (Pad s p e) = f =<< liftM (Pad s p) (transformExp f e)
transformExp f (Sym s v c i e) = f =<< liftM (Sym s v c i) (transformExp f e)
transformExp f (Stmts e1 e2) = do 
    e1' <- transformExp f e1
    e2' <- transformExp f e2
    f $ Stmts e1' e2'
transformExp f e = f e

{- FIXME: Figure out how to get this working without a monad, and make it castV -}
expToEvalVal :: Exp -> Eval Val
expToEvalVal exp = do
    obj <- createObject (mkType "Code::Exp") []
    return $ VObject obj{ objOpaque = Just $ toDyn exp }

fromVals :: (Value n) => Val -> Eval [n]
fromVals v = mapM fromVal =<< fromVal v

extractPlaceholderVarsExp :: Exp -> ([Exp], Set Var) -> ([Exp], Set Var)
extractPlaceholderVarsExp ex (exps, vs) = (ex':exps, vs')
    where
    (ex', vs') = extractPlaceholderVars ex vs

{-| Deduce the placeholder vars ($^a, $^x etc.) used by a block). -}
extractPlaceholderVars :: Exp -> Set Var -> (Exp, Set Var)
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
        "when"  -> Set.insert (cast "$_") vs'
        "given" -> Set.delete (cast "$_") vs'
        _       -> vs'
extractPlaceholderVars (Var var) vs
    | TImplicit <- v_twigil var
    , var' <- var{ v_twigil = TNil }
    = (Var var', Set.insert var' vs)
    | var == cast "$_"
    = (Var var, Set.insert var vs)
    | otherwise
    = (Var var, vs)
extractPlaceholderVars (Ann ann ex) vs = ((Ann ann ex'), vs')
    where
    (ex', vs') = extractPlaceholderVars ex vs
-- extractPlaceholderVars (Pad scope pad ex) vs = ((Pad scope pad ex'), vs')
--     where
--     (ex', vs') = extractPlaceholderVars ex vs
extractPlaceholderVars (Sym scope var flags ini ex) vs = ((Sym scope var flags ini ex'), vs')
    where
    (ex', vs') = extractPlaceholderVars ex vs
extractPlaceholderVars exp vs = (exp, vs)

envPos' :: Env -> Pos
envPos' = envPos

envWant :: Env -> String
envWant env =
    showCxt (envContext env) ++ (if envLValue env then ", LValue" else "")
    where
    showCxt CxtVoid         = "Void"
    showCxt (CxtItem typ)   = "Scalar (" ++ showType typ ++ ")"
    showCxt (CxtSlurpy typ) = "List (" ++ showType typ ++ ")"

refreshPad :: Pad -> Eval Pad
refreshPad pad = do
    fmap listToPad $ forM (padToList pad) $ \(name, entry) -> do
        -- warn "Refreshing pad entry" (name, entry)
        entry' <- case entry of
            PELexical{ pe_proto = proto } -> stm $ do
                ref     <- cloneRef proto
                tvar'   <- newTVar ref
                return entry{ pe_store = tvar' }
            _ -> return entry
        return (name, entry')

{-|
Retrieve the global 'Pad' from the current evaluation environment.

'Env' stores the global 'Pad' in an STM variable, so we have to @asks@
'Eval'\'s @ReaderT@ for the variable, then extract the pad itself from the
STM var.
-}
askGlobal :: Eval Pad
askGlobal = do
    glob <- asks (mp_pad . envGlobal)
    stm $ readTVar glob

writeVar :: Var -> Val -> Eval ()
writeVar var val
    | isLexicalVar var  = doWriteVar (asks envLexical)
    | otherwise         = doWriteVar askGlobal
    where
    doWriteVar askPad = do
        pad <- askPad
        case lookupPad var pad of
            Just PEConstant{} -> fail $ "Cannot rebind constant: " ++ show var
            Just c -> do
                ref <- stm $ readTVar (pe_store c)
                writeRef ref val
            _  -> fail $ "Cannot bind to non-existing variable: " ++ show var

readVar :: Var -> Eval Val
readVar var
    | isLexicalVar var = do
        lex <- asks envLexical
        case findSym var lex of
            Just action -> stm action >>= readRef
            _           -> return undef
    | otherwise = do
        glob <- askGlobal
        case findSym var glob of
            Just action -> stm action >>= readRef
            _           -> return undef

{-|
The \'empty expression\' is just a no-op ('Noop').
-}
emptyExp :: Exp
emptyExp = Noop

retControl :: VControl -> Eval a
retControl = retShift . VControl

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
forceRef r = die "Cannot forceRef" r

dumpRef :: VRef -> Eval Val
dumpRef (MkRef (ICode cv)) = do
    vsub <- code_fetch cv
    return (VStr $ "(MkRef (ICode $ " ++ show vsub ++ "))")
dumpRef (MkRef (IScalar sv)) | scalar_iType sv == mkType "Scalar::Const" = do
    sv <- scalar_fetch sv
    return (VStr $ "(MkRef (IScalar $ " ++ show sv ++ "))")
dumpRef ref = return (VStr $ "(unsafePerformIO . newObject $ mkType \"" ++ showType (refType ref) ++ "\")")

-- Reduce a VRef in rvalue context. 
readRef :: VRef -> Eval Val
readRef (MkRef (IScalar sv)) = scalar_fetch sv
readRef (MkRef (ICode cv)) = do
    vsub <- code_fetch cv
    return $ VCode vsub
readRef (MkRef (IHash hv)) = do
    pairs <- hash_fetch hv
    return $ VList $ map (\(k, v) -> castV (castV k, v)) (Map.assocs pairs)
readRef (MkRef (IArray av)) = do
    vals <- array_fetch av
    return $ VList vals

-- XXX - This case is entirely bogus; but no time to fix it now.
readRef (MkRef (IPair pv)) = do
    (k, v) <- pair_fetch pv
    return $ VList [k, v]

readRef (MkRef (IHandle io)) = return . VHandle =<< handle_fetch io
readRef (MkRef (IRule rx)) = return . VRule =<< rule_fetch rx
readRef (MkRef (IThunk tv)) = readRef =<< fromVal =<< thunk_force tv
readRef (MkRef (IVal v)) = do
    cxt <- asks envContext
    v ./ cxt

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
writeRef r _ = die "Cannot writeRef" r

cloneRef :: VRef -> STM VRef
cloneRef (MkRef x) = fmap MkRef (cloneIVar x)

clearRef :: VRef -> Eval ()
clearRef (MkRef (IScalar s)) = scalar_store s undef
clearRef (MkRef (IArray s))  = array_clear s
clearRef (MkRef (IHash s))   = hash_clear s
clearRef (MkRef (IPair s))   = pair_storeVal s undef
clearRef (MkRef (IThunk tv)) = clearRef =<< fromVal =<< thunk_force tv
clearRef r = die "Cannot clearRef" r

{-# SPECIALISE newObject :: Type -> Eval VRef #-}
{-# SPECIALISE newObject :: Type -> IO VRef #-}
newObject :: (MonadSTM m, MonadIO m) => Type -> m VRef
newObject typ = case showType typ of
    "Any"       -> io $ fmap scalarRef $ newTVarIO undef
    "Item"      -> io $ fmap scalarRef $ newTVarIO undef
    "Scalar"    -> io $ fmap scalarRef $ newTVarIO undef
    "Array"     -> io $ do
        iv  <- newTVarIO [::]
        return $ arrayRef (MkIArray iv)
    "Hash"      -> do
        h   <- io (H.new (==) H.hashString)
        return $ hashRef (h :: IHash)
    "Sub"       -> newObject $ mkType "Code"
    "Routine"   -> newObject $ mkType "Code"
    "Method"    -> newObject $ mkType "Code"
    "Submethod" -> newObject $ mkType "Code"
    "Code"      -> return $! codeRef $ mkPrim
        { subAssoc = AIrrelevantToParsing
        , subBody  = Prim . const $ fail "Cannot use Undef as a Code object"
        }
    "Type"      -> io $ fmap scalarRef $ newTVarIO undef
    "Pair"      -> do
        key <- newObject (mkType "Scalar")
        val <- newObject (mkType "Scalar")
        return $ MkRef (IPair (VRef key, VRef val))
    "Regex"     -> io $ fmap scalarRef $ newTVarIO undef -- XXX Wrong
    "Capture"   -> io $ fmap scalarRef $ newTVarIO undef -- XXX Wrong
    _           -> fail ("Class prototype occured where its instance object expected: " ++ showType typ)

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
doPair (VRef x) _ = die "Cannot cast into Pair" x
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
doHash val@(VRef _) _ = die "Cannot cast into Hash" val
doHash val f = do
    hv  <- fromVal val
    return $ f (hv :: VHash)

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
doArray val@(VRef _) _ = die "Cannot cast into Array" val
doArray (VMatch m) f = do
    return $ f (matchSubPos m)
doArray val f = do
    av  <- fromVal val
    return $ f (av :: VArray)

readIVar :: IVar v -> Eval v
readIVar (IScalar x) = scalar_fetch x
readIVar (IPair x)   = pair_fetch x
readIVar (IArray x)  = array_fetch x
readIVar (IHash x)   = hash_fetch x
readIVar _ = fail "readIVar"

cloneIVar :: IVar v -> STM (IVar v)
cloneIVar (IScalar x) = fmap IScalar $ scalar_clone x
cloneIVar (IArray x)  = fmap IArray  $ array_clone x
cloneIVar (IHash x)   = fmap IHash   $ hash_clone x
cloneIVar (ICode x)   = fmap ICode   $ code_clone x
cloneIVar x = return x

writeIVar :: IVar v -> v -> Eval ()
writeIVar (IScalar x) = scalar_store x
writeIVar (IArray x) = array_store x
writeIVar (IHash x) = hash_store x
writeIVar _ = fail "writeIVar"

refType :: VRef -> Type
refType (MkRef x) = object_iType x

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
newScalar = stm . (fmap IScalar) . newTVar

newArray :: (MonadSTM m) => VArray -> m (IVar VArray)
newArray vals = stm $ do
    tvs <- mapM newScalar vals
    iv  <- newTVar (toP tvs)
    return $ IArray (MkIArray iv)

newHash :: (MonadSTM m) => VHash -> m (IVar VHash)
newHash hash = do
    --stm $ unsafeIOToSTM $ putStrLn "new hash"
    ihash <- stm . unsafeIOToSTM $ H.fromList H.hashString (map (\(a,b) -> (a, lazyScalar b)) (Map.toList hash))
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

------------------------------------------------------------------------
anyFromVal :: forall a. Typeable a => Val -> a
anyFromVal v = case fromTypeable (fromVal v :: Eval PerlSV) of
    Just f  -> f :: a
    _       -> error "anyFromVal failed!"

intCast :: Num b => Val -> Eval b
intCast x = fmap fromIntegral (fromVal x :: Eval VInt)


showVal :: Val -> String
showVal = show

defaultArrayParam :: Param
defaultHashParam :: Param
defaultScalarParam :: Param

defaultArrayParam   = buildParam "" "*" "@_" (Val VUndef)
defaultHashParam    = buildParam "" "*" "%_" (Val VUndef)
defaultScalarParam  = buildParam "" "?" "$_" (Var $ cast "$OUTER::_")


-- Class: Value

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
        str <- Val.asStr v
        fail $ "Cannot cast from VV (" ++ cast str ++ ") to " ++ errType (undefined :: n)
    fromSV :: PerlSV -> Eval n
    fromSV sv = do
        str <- io $ svToVStr sv
        fail $ "Cannot cast from SV (" ++ str ++ ") to " ++ errType (undefined :: n)
    castV :: n -> Val
    castV x = VOpaque (MkOpaque x) -- error $ "Cannot cast into Val"


-- Instances: Value

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
    fromVal (VCode c)  = return $ codeRef c
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
        l <- io $ H.toList (objAttrs o)
        fmap Map.fromList . forM l $ \(k, ivar) -> do
            v <- readIVar ivar
            return (k, v)
    fromVal VType{} = return Map.empty -- ::Hash<foo>
    fromVal (VRef r) = fromVal =<< readRef r
    fromVal v = do
        list <- fromVal v
        fmap Map.fromList $ forM list $ \(k, v) -> do
            str <- fromVal k
            return (str, v)
    doCast v = castFailM v "VHash"

instance Value [VPair] where
    fromVal VUndef = return []
    fromVal v = do
        list <- fromVals v
        doFrom $ concat list
        where
        doFrom :: [Val] -> Eval [VPair]
        doFrom [] = return []
        doFrom [_] = fail $ "Odd number of elements found where hash expected: " ++ show v
        doFrom (k:v:list) = do
            rest <- doFrom list
            return ((k, v):rest)
    doCast v = castFailM v "Hash"

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
    doCast (VType t) = return $ mkPrim
        { subName     = cast t
        , subParams   = [buildParam "Any" "*" "@?0" (Val VUndef), buildParam "Any" "*" "%?0" (Val VUndef)]
        , subReturns  = mkType "Scalar::Perl5"
        , subBody     = Prim $ \(p:n:_) -> do
            evl <- asks envEval
            evl (App (_Var "&new") (Just $ Val (VType t)) [Syn "|" [Val p], Syn "|" [Val n]])
        }
    doCast (VList [VCode b]) = return b -- XXX Wrong
    doCast v = castFailM v "VCode"

instance Value VBool where
    castV = VBool
    fromSV sv = io $ svToVBool sv
    fromVV vv = fmap cast (Val.asBit vv)
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
    fromVV vv = fmap cast (Val.asInt vv)
    fromSV sv = io $ svToVInt sv
    doCast (VInt i)     = return $ i
    doCast x            = fmap truncate (fromVal x :: Eval VRat)

instance Value VRat where
    castV = VRat
    fromSV sv = io $ svToVNum sv
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
    fromVV vv = fmap cast (Val.asNum vv)
    fromSV sv = io $ svToVNum sv
    doCast VUndef       = return $ 0
    doCast VType{}      = return $ 0
    doCast (VBool b)    = return $ if b then 1 else 0
    doCast (VInt i)     = return $ fromIntegral i
    doCast (VRat r)     = return $ realToFrac r
    doCast (VNum n)     = return $ n
    doCast (VComplex (r :+ _)) = return $ r
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
    doCast t@VThread{}   = fmap read (fromVal t)
    doCast (VMatch m)    = fromVal (VStr $ matchStr m)
    doCast v = castFailM v "VNum"

instance Value Ordering where
    castV x = VInt $ case x of
        LT -> -1
        EQ -> 0
        GT -> 1
    doCast x = do
        n <- fromVal x :: Eval VInt
        return $ case signum n of
            -1  -> LT
            0   -> EQ
            1   -> GT
            _   -> error "signum: impossible"

instance Value VComplex where
    castV = VComplex
    doCast (VComplex x) = return x
    doCast x            = fmap (:+ 0) (fromVal x :: Eval VNum)

instance Value ID where
    castV = VStr . cast
    fromSV sv = fmap cast (io $ svToVStr sv)
    fromVV vv = fmap cast (Val.asStr vv)
    fromVal = fmap (cast :: VStr -> ID) . fromVal
    doCast = fmap (cast :: VStr -> ID) . doCast

instance Value VStr where
    castV = VStr
    fromSV sv = io $ svToVStr sv
    fromVV vv = fmap cast (Val.asStr vv)
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
    doCast (VComplex (r :+ i)) = return $ showNum r ++ " + " ++ showNum i ++ "i"
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
    fromVal val = io $ newSVval val
    doCast v = castFailM v "PerlSV"

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

instance Value VThread where
    castV = VThread
    doCast (VThread x)  = return $ x
    doCast v = castFailM v "VThread"

instance Value VProcess where
    castV = VProcess
    doCast (VProcess x)  = return $ x
    doCast v = castFailM v "VProcess"

instance Value Int where
    fromSV sv = io $ svToVInt sv
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

instance Value VScalar where
    fromSV = return . PerlSV
    fromVV = cast . fmap VV . Val.itemVal
    fromVal (VRef r) = fromVal =<< readRef r
    fromVal v = return v
    doCast v = return v
    castV = id -- XXX not really correct; need to referencify things

instance Value Exp where
    {- Val -> Eval Exp -}
    fromVal val = do
        obj <- fromVal val
        return $ fromObject obj
    {- Exp -> Val -}
    {- castV exp = VObject (createObject (mkType "Code::Exp") [("theexp", exp)]) -}
    doCast v = castFailM v "Exp"

instance Value VOpaque where
    fromVal (VOpaque o) = return o
    fromVal v = return $ MkOpaque v
    castV (MkOpaque x) = castV x
    doCast v = castFailM v "VOpaque"

-- Instances: Others

instance Unwrap [Exp] where
    unwrap = map unwrap

instance Unwrap Exp where
    unwrap (Ann _ exp)      = unwrap exp
    -- unwrap (Pad _ _ exp)    = unwrap exp
    unwrap (Sym _ _ _ _ exp)= unwrap exp
    unwrap x                = x

instance Eq VOpaque where
    (MkOpaque x) == (MkOpaque y) = castV x == castV y

instance Typeable VOpaque where
    typeOf (MkOpaque x) = typeOf x

instance Ord VOpaque where
    compare x y = castV x `compare` castV y

instance Show VOpaque where
    show (MkOpaque x) = show x

instance Typeable1 IVar where
    typeOf1 (IScalar x) = typeOf x
    typeOf1 (IArray  x) = typeOf x
    typeOf1 (IHash   x) = typeOf x
    typeOf1 (ICode   x) = typeOf x
    typeOf1 (IHandle x) = typeOf x
    typeOf1 (IRule   x) = typeOf x
    typeOf1 (IThunk  x) = typeOf x
    typeOf1 (IPair   x) = typeOf x
    typeOf1 (IVal    x) = typeOf x

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
        IVal    x -> show x
        where
        showAddr x = showAddressOf (showType (refType ref)) x

instance Typeable VRef where
    typeOf (MkRef x) = typeOf x

instance Eq VRef where
    x == y = addressOf x == addressOf y
instance Ord VRef where
    compare x y = compare (addressOf x) (addressOf y)

instance Typeable a => Show (IVar a) where
    show ivar = show (MkRef ivar)
