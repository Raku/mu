{-# OPTIONS_GHC -cpp -fglasgow-exts -fno-warn-orphans -funbox-strict-fields -fallow-overlapping-instances -fparr #-}

{-|
    Abstract syntax tree.

>   Tall ships and tall kings
>   Three times three.
>   What brought they from the foundered land
>   Over the flowing sea?
>   Seven stars and seven stones
>   And one white tree.
-}

module Pugs.AST (
    evalExp, evalExp_, readCodesFromRef,
    genSym, genMultiSym, genSymScoped, genPadEntryScoped, mkPadMutator,
    strRangeInf, strRange, strInc,
    mergeStmts, isEmptyParams, isCompileTime,
    newPackage, newType, newMetaType, typeMacro, isScalarLValue,
    filterPrim, filterUserDefinedPad, typeOfParam, listVal, isImmediateMatchContext,
    (./), defaultScalarPad, envPosStack,

    module Pugs.AST.Internals,
    module Pugs.AST.Prag,
    module Pugs.AST.Pos,
    module Pugs.AST.Scope,
    module Pugs.AST.SIO,
    module Pugs.AST.Pad,
    module Pugs.Val,
    module Pugs.Class
) where
import Pugs.Internals
import Pugs.Types
import qualified Data.Map as Map
import qualified Data.Set as Set
import Pugs.AST.Internals.Instances ()
import Pugs.AST.Internals
import Pugs.AST.Prag
import Pugs.AST.Pos
import Pugs.AST.Scope
import Pugs.AST.SIO
import Pugs.AST.Pad
import Pugs.Val hiding (Val, Param, listVal) -- (val, castVal, formatVal, PureBit, PureBool, PureStr, PureInt, PureNum, Capt(..), ValCapt, Feed(..), ValFeed, emptyFeed, Sig(..), SigParam(..), ParamAccess(..), ParamDefault(..))
import qualified Pugs.Val as Val
import Pugs.Meta ()
import Pugs.Class (Boxable(..), ResponderInterface(..), Invocant(..), AnyResponder(..), MethodInvocation(..), ivDispatch)

instance Value (Val.Val) where
    fromVV = return
    fromSV = return . mkVal
    fromVal v = case v of
        VV x@(MkInvocant x' _) -> case fromTypeable x' of
            Just v  -> fromVal v
            _       -> return x
        VUndef      -> return . mkVal $ UUndef
        VBool x     -> return . mkVal $ ((cast x) :: Val.PureBit)
        VInt x      -> return . mkVal $ ((cast x) :: Val.PureInt)
        VNum x      -> return . mkVal $ ((cast x) :: Val.PureNum)
        VRat x      -> return . mkVal $ ((cast x) :: Val.PureNum)
        VStr x      -> return . mkVal $ ((cast x) :: Val.PureStr)
        PerlSV x    -> return . mkVal $ x
        _           -> return . mkVal $ v
    doCast = fromVal
    castV  = VV

data OldValResponder = OldValResponder deriving Typeable
instance ResponderInterface Eval OldValResponder where
    dispatch _          = dispatchOldVal
    fromMethodList _    = return OldValResponder

instance Boxable Val where
    mkVal sv = MkInvocant sv (MkResponder (return OldValResponder))

dispatchOldVal :: Val.Val -> Call -> Eval Val.Val
dispatchOldVal inv call
    | meth == nullID = do
        typ <- evalValType =<< castVal inv
        (fromVal =<<) . evalExp $ _Var (':':'*':showType typ)
    | otherwise      = do 
        inv' <- castVal inv
        (fromVal =<<) . evalExp $ App
            (_Var ('&':cast meth))
            (Just $ Val inv')
            [Syn "|" [Val . VV . mkVal $ mi_arguments call]]
    where
    meth = mi_name call

{-|
Return an infinite (lazy) Haskell list of the given string and its
successors. 'strInc' is used to determine what the \'next\' string is.

Used to implement the @...@ infinite-range operator on strings.
-}
strRangeInf :: String -> [String]
strRangeInf s = (s:strRangeInf (strInc s))

{-|
Return a range of strings from the first argument to the second, inclusive
(as a Haskell list). 'strInc' is used to determine what the \'next\' string 
is.

Used to implement the @..@ range operator on strings.
-}
strRange :: String -> String -> [String]
strRange s1 s2
    | s1 == s2              = [s2]
    | length s1 > length s2 = []
    | length s1 < length s2 = (s1:strRange (strInc s1) s2)
    | s1 >  s2              = []
    | otherwise             = (s1:strRange (strInc s1) s2)

{-|
Find the successor of a string (i.e. the next string \'after\' it).
Special rules are used to handle wraparound for strings ending in an
alphanumeric character; otherwise the last character is simply incremented 
using 'succ'.
-}
strInc :: String -> String
strInc []       = "1"
strInc "z"      = "aa"
strInc "Z"      = "AA"
strInc "9"      = "10"
strInc str
    | x == 'z'  = strInc xs ++ "a"
    | x == 'Z'  = strInc xs ++ "A"
    | x == '9'  = strInc xs ++ "0"
    | otherwise = xs ++ [succ x]
    where
    x   = last str
    xs  = init str

{-|
Evaluate the given expression, using the currently active evaluator
(as given by the 'envEval' slot of the current 'Env').
-}
evalExp :: Exp -> Eval Val
evalExp exp = do
    evl <- asks envEval
    evl exp

evalExp_ :: Exp -> Eval ()
evalExp_ exp = do
    evalExp exp
    return ()

genMultiSym :: MonadSTM m => Var -> VRef -> EntryFlags -> m PadMutator
genMultiSym var = case v_sigil var of
    SCode -> \ref flags -> do
        case ref of
            MkRef (ICode c) -> do
                let var' = var{ v_longname = _cast (cast (code_params c)) }
                genSymScoped SMy var' ref flags
            _               -> die "Cannot generate multi variants of non-code object" ref
    _           -> const $ die "Cannot generate multi variants of variable" var

isStaticScope :: Scope -> Bool
isStaticScope SOur    = True
isStaticScope SState  = True
isStaticScope _       = False

genPadEntryScoped :: MonadSTM m => Scope -> VRef -> EntryFlags -> m PadEntry
genPadEntryScoped scope ref flags
    | SConstant <- scope = do
        return (PEConstant typ ref flags)
    | isStaticScope scope = stm $ do
        tvar    <- newTVar ref
        return (PEStatic typ ref flags tvar)
    | otherwise = stm $ do
        tvar    <- newTVar ref
        return (PELexical typ ref flags tvar)
    where
    typ = refType ref

{-# NOINLINE genSymScoped #-}
genSymScoped :: MonadSTM m => Scope -> Var -> VRef -> EntryFlags -> m PadMutator
genSymScoped scope var ref flags = do
    entry <- genPadEntryScoped scope ref flags
    return (mkPadMutator var entry ref)

mkPadMutator :: Var -> PadEntry -> VRef -> PadMutator
mkPadMutator var entry ref (MkPad map)
    | v_longname var /= nullID, MkRef (ICode c) <- ref
    = let   var'        = var{ v_longname = nullID }
            protoEntry  = PEConstant
                { pe_type  = pe_type entry
                , pe_proto = MkRef (ICode protoCode)
                , pe_flags = pe_flags entry
                }
            protoCode = MkMultiCode
                { mc_type       = pe_type entry
                , mc_subtype    = code_type c
                , mc_assoc      = code_assoc c
                , mc_signature  = code_params c
                , mc_variants   = Set.singleton var
                }
            merge :: PadEntry -> PadEntry -> PadEntry
            merge _ PEConstant{ pe_proto = MkRef (ICode oldCV) }
                | Just (mc :: VMultiCode) <- fromTypeable oldCV
                = protoEntry
                    { pe_proto = MkRef . ICode $ protoCode
                        { mc_assoc      = code_assoc c `mappend` code_assoc mc
                        , mc_variants   = Set.insert var (mc_variants mc)
                        , mc_signature  = if length (mc_signature mc) == length (code_params c)
                            then code_params c
                            else [defaultArrayParam]
                        }
                    }
            merge _ old = old -- sub overrides multi -- XXX - error?
       in MkPad (Map.insertWith' merge var' protoEntry (Map.insert var entry map))
    | otherwise = MkPad (Map.insert var entry map)

{-|
Create a lexical 'Pad'-transforming transaction that will install a symbol
mapping from a name to a thing, in the 'Pad' it is applied to.
-}
genSym :: MonadSTM m => Var -> VRef -> m PadMutator
genSym var ref = genSymScoped scope var ref $! case v_twigil var of
    TMagical    -> mempty{ ef_isContext = True }
    _           -> mempty
    where
    scope = case v_twigil var of
        TMagical -> SConstant
        _        -> SMy

{-|
Tests whether an expression is /simple/, per the definition of S03.
On the LHS of assignment, those expressions incurs a scalar context.
-}
isScalarLValue :: Exp -> Bool
isScalarLValue x = case x of
    Ann Parens _    -> False
    Var var | SScalar <- v_sigil var -> True
    Syn "${}" _     -> True -- XXX - Change tp App("&prefix:<$>") later
    Syn "$::()" _   -> True
    _               -> False

opSet :: VarCateg -> [String] -> Set Var
opSet cat posts = Set.fromList $ map doMakeVar posts
    where
    doMakeVar name = MkVar
        { v_sigil   = SCode
        , v_twigil  = TNil
        , v_package = emptyPkg
        , v_categ   = cat
        , v_name    = cast name
        , v_meta    = MNil
        , v_longname= nullID
        }

coercePrefixOps, simplePrefixOps, simplePostfixOps, simpleInfixOps :: Set Var
coercePrefixOps = opSet C_prefix [ "!","+","-","~","?","$" ]
simplePrefixOps = opSet C_prefix
    [ "++","--"
    , "$","&","+^","~^","?^","\\","^","="
    ]
simplePostfixOps = opSet C_postfix ["++", "--"]
simpleInfixOps = opSet C_infix
    [ "**"
    , "**="
    , "*","/","%","x","+&","+<","+>","~&","~<","~>"
    , "*=","/=","%=","x=","+&=","+<=","+>=","~&=","~<=","~>="
    , "+","-","~","+|","+^","~|","~^"
    , "+=","-=","~=","+|=","+^=","~|=","~^="
    ]




-- Stmt is essentially a cons cell
-- Stmt (Stmt ...) is illegal
mergeStmts :: Exp -> Exp -> Exp
mergeStmts (Stmts x1 x2) y = mergeStmts x1 (mergeStmts x2 y)
mergeStmts Noop y@(Stmts _ _) = y
mergeStmts (Sym scope name flag init x) y = Sym scope name flag init (mergeStmts x y)
mergeStmts (Syn "package" [kind, pkg@(Val (VStr _))]) y =
    Syn "namespace" [kind, pkg, y]
mergeStmts x@(Ann ann (Syn syn _)) y | isImplicitTopic syn =
    mergeStmts (Ann ann (App (_Var "&infix:~~") Nothing [_Var "$_", x])) y
mergeStmts x y@(Ann ann (Syn syn _)) | isImplicitTopic syn =
    mergeStmts x (Ann ann (App (_Var "&infix:~~") Nothing [_Var "$_", y]))
mergeStmts x@(Ann _ (Syn "sub" [Val (VCode sub)])) y | subType sub == SubBlock =
    -- bare Block in statement level; annul all its parameters and run it!
    mergeStmts (Syn "block" [x]) y
mergeStmts x y@(Ann _ (Syn "sub" [Val (VCode sub)])) | subType sub == SubBlock =
    -- bare Block in statement level; annul all its parameters and run it!
    mergeStmts x (Syn "block" [y])
mergeStmts x (Stmts y Noop) = mergeStmts x y
mergeStmts x (Stmts Noop y) = mergeStmts x y
mergeStmts x y = Stmts x y

isImplicitTopic :: String -> Bool
isImplicitTopic "subst" = True
isImplicitTopic "match" = True
isImplicitTopic "trans" = True
isImplicitTopic "//"    = True
isImplicitTopic _       = False

isEmptyParams :: [Param] -> Bool
isEmptyParams [] = True
isEmptyParams [x]
    | var <- paramName x
    , _underscore == v_name var
    , emptyPkg    == v_package var
    , TNil        == v_twigil var
    = True
isEmptyParams _ = False

_underscore :: ID
_underscore = cast "_" 

newPackage :: String -> String -> [String] -> [String] -> Exp
newPackage cls name classes roles = Stmts metaObj (newType name)
    where
    metaObj = _Sym SOur (':':'*':name) mempty (
        App (_Var "&HOW::new")
            (Just $ Val (VType $ mkType cls))
            [ Syn "named"
                [ Val (VStr "is")
                , Val (VList $ map VStr classes)
                ]
            , Syn "named"
                [ Val (VStr "does")
                , Val (VList $ map VStr roles)
                ]
            , Syn "named"
                [ Val (VStr "name")
                , Val (VStr name)
                ]
            , Syn "named"
                [ Val (VStr "attrs")
                , Syn "\\{}" [Noop]
                ]
            ]
            ) Noop

newType :: String -> Exp
newType name = _Sym SOur ('&':'*':termName) mempty (typeMacro name (Val . VType . mkType $ name)) Noop
    where
    termName = "term:" ++ name


newMetaType :: String -> Exp
newMetaType name = _Sym SOur ('&':'*':termName) mempty (typeMacro name (_Var (':':'*':name))) Noop
    where
    termName = "term:" ++ name

typeMacro :: String -> Exp -> Exp
typeMacro name exp = Syn "sub" . (:[]) . Val . VCode $ MkCode
    { isMulti       = True
    , subName       = cast name
    , subOuterPads  = []
    , subInnerPad   = emptyPad
--  , subLexical    = emptyPad
    , subStarted    = Nothing
    , subPackage    = emptyPkg
    , subType       = SubMacro
    , subAssoc      = ANil
    , subReturns    = typ
    , subLValue     = False
    , subParams     = [ defaultArrayParam, defaultHashParam ]
    , subBindings   = []
    , subSlurpLimit = []
    , subBody       = Prim $ \v -> do
        list <- mapM fromVals v :: Eval [VList]
        case concat list of
            []  -> expToEvalVal $ exp
            xs  -> die ("Cannot coerce to " ++ name) xs
    , subCont          = Nothing
    , subTraitBlocks   = emptyTraitBlocks
    }
    where
    typ = mkType name

{- utilities for filtering out primitives from an environmet, useful for
 - CodeGen and Pugs::Internals::emit_yaml -}

filterPrim :: MPad -> Eval Pad
filterPrim glob = do
    MkPad pad   <- readMPad glob
    fmap (MkPad . Map.fromAscList . catMaybes) . mapM checkPrim $ Map.toAscList pad

checkPrim :: (Var, PadEntry) -> Eval (Maybe (Var, PadEntry))
checkPrim e@(var, entry)
    | SType <- v_sigil var, isGlobalVar var = return Nothing
    | otherwise = do
        rv <- isPrim =<< readPadEntry entry
        return (if rv then Nothing else Just e)

isPrim :: VRef -> Eval Bool
isPrim vref = do
    case vref of
        MkRef (ICode cv)    -> fmap (isPrimVal . VCode) (code_fetch cv)
        MkRef (IScalar sv)  -> fmap isPrimVal (scalar_fetch sv)
        _                   -> return False
    where
    isPrimVal (VCode MkCode{ subBody = Prim _ }) = True
    isPrimVal _ = False

{-|
Filter out reserved symbols from the specified Pad.
-}
filterUserDefinedPad :: Pad -> Pad
filterUserDefinedPad (MkPad pad) = MkPad $ Map.filterWithKey doFilter pad
    where
    doFilter key _ = (not . Set.member key) _reserved

{-|
Symbols which are reserved for the current interpreter or compiler instance and
should not be set from the preamble or other sources. See @filterUserDefinedPad@.
-}
_reserved :: Set Var
_reserved = Set.fromList . cast . words $
    "@*ARGS @*INC %*INC $*PUGS_HAS_HSPLUGINS $*EXECUTABLE_NAME " ++
    "$*PROGRAM_NAME $*PID $*UID $*EUID $*GID $*EGID @*CHECK @*INIT $*IN " ++
    "$*OUT $*ERR $*ARGS $/ %*ENV $*CWD @=POD $=POD $?PUGS_VERSION " ++
    "$*OS %?CONFIG $*_ $*AUTOLOAD $*PUGS_VERSION $*BASETIME"

typeOfParam :: Param -> Type
typeOfParam p = case v_sigil (paramName p) of
    SScalar -> typeOfCxt (paramContext p)
    s       -> typeOfSigil s

listVal :: Val -> [Val]
listVal (VList xs)  = xs
listVal x           = [x]

isImmediateMatchContext :: Eval Bool
isImmediateMatchContext = do
    env <- ask
    let cxt = envContext env
        typ = typeOfCxt cxt
    return (cxt == CxtVoid || (any (\x -> isaType x typ) ["Bool", "Num", "Str"]))

(./) :: ((:>:) Call a) => Val -> a -> Eval Val
(VV vv) ./ y = vvToVal =<< ivDispatch vv (cast y)
x ./ y       = do
    vv <- fromVal x
    vvToVal =<< ivDispatch vv (cast y)

instance ((:>:) Call) Cxt where
    cast CxtSlurpy{} = __LIST__
    cast _           = __ITEM__

__LIST__ :: Call
__LIST__ = cast "LIST"

__ITEM__ :: Call
__ITEM__ = cast "ITEM"

readCodesFromRef :: VRef -> Eval [VCode]
readCodesFromRef (MkRef (ICode c))
    | Just (mc :: VMultiCode) <- fromTypeable c = do
        let names@(pivot:_) = Set.elems (mc_variants mc)
        rvs <- fmap concat . forM names $ \var -> do
            ref  <- fromVal =<< readVar var
            readCodesFromRef ref
        if not (isLexicalVar pivot) then return rvs else do
            -- Lexical multis must also include global variants.
            cvGlobal <- readVar (toGlobalVar pivot{ v_longname = nullID })
            if not (defined cvGlobal) then return rvs else do
                rvsGlobal <- readCodesFromRef =<< fromVal cvGlobal
                return (rvsGlobal ++ rvs)
    | Just (cv :: VCode) <- fromTypeable c = return [cv]
readCodesFromRef ref = do
    code <- fromVal =<< readRef ref
    readCodesFromRef (MkRef (ICode (code :: VCode)))

isCompileTime :: Env -> Bool
isCompileTime = isJust . envCompPad


{-# NOINLINE defaultScalarPadStore #-}
defaultScalarPadStore :: TVar VRef
defaultScalarPadStore = unsafePerformIO (newTVarIO defaultScalarRef)

defaultScalarRef :: VRef
defaultScalarRef = scalarRef undef

defaultScalarPad :: Pad
defaultScalarPad = mkPad [(varTopic, PELexical anyType defaultScalarRef mempty defaultScalarPadStore)]

envPosStack :: Env -> [Pos]
envPosStack env = envPos env : maybe [] envPosStack (envCaller env)

