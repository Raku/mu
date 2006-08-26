{-# OPTIONS_GHC -cpp -fglasgow-exts -fno-warn-orphans -funbox-strict-fields -fallow-overlapping-instances #-}

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
    evalExp,
    genMultiSym, genSym,
    strRangeInf, strRange, strInc,
    mergeStmts, isEmptyParams,
    newPackage, newType, newMetaType, typeMacro, isScalarLValue,
    filterPrim, filterUserDefinedPad,

    module Pugs.AST.Internals,
    module Pugs.AST.Prag,
    module Pugs.AST.Pos,
    module Pugs.AST.Scope,
    module Pugs.AST.SIO,
    module Pugs.AST.Pad,
    module Pugs.Val,
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
import Pugs.Val (val, PureStr, PureInt, PureNum, Capt(..), ValCapt, Feed(..), ValFeed, emptyFeed)

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

{-|
Create a 'Pad'-transforming transaction that will install a symbol
definition in the 'Pad' it is applied to, /alongside/ any other mappings
of the same name. This is to allow for overloaded (i.e. multi) subs,
where one sub name actually maps to /all/ the different multi subs.
(Is this correct?)
-}
genMultiSym :: MonadSTM m => Var -> VRef -> m PadMutator
genMultiSym name ref = do
    --trace ("installing multi: " ++ name) $ return ()
    tvar    <- liftSTM $ newTVar ref
    fresh   <- liftSTM $ newTVar True
    return $ \(MkPad map) -> MkPad $
        Map.insertWith mergePadEntry name (MkEntryMulti [(fresh, tvar)]) map

{-|
Create a 'Pad'-transforming transaction that will install a symbol
mapping from a name to a thing, in the 'Pad' it is applied to.
Unlike 'genMultiSym', this version just installs a single definition
(right?), shadowing any earlier or outer definition.
-}
genSym :: MonadSTM m => Var -> VRef -> m PadMutator
genSym var ref = do
    --trace ("installing: " ++ name) $ return ()
    tvar    <- liftSTM $ newTVar ref
    fresh   <- liftSTM $ newTVar True
    return $ \(MkPad map) -> MkPad $ Map.insert var (MkEntry (fresh, tvar)) map

{-|
Tests whether an expression is /simple/, per the definition of S03.
On the LHS of assignment, those expressions incurs a scalar context.
-}
isScalarLValue :: Exp -> Bool
isScalarLValue x = case x of
    Ann Parens _    -> False
    Ann _ exp       -> isScalarLValue exp
    Pad _ _ exp     -> isScalarLValue exp
    Sym _ _ exp     -> isScalarLValue exp
    Var var | SScalar <- v_sigil var -> True
    Syn "${}" _     -> True -- XXX - Change tp App("&prefix:<$>") later
    Syn "$::()" _   -> True
    Syn "[]" [_, y] -> isSIMPLE y
    Syn "{}" [_, y] -> isSIMPLE y
    Val VList{}     -> False
    Val{}           -> True
    _               -> False
    where
    isSIMPLE x = case unwrap x of
        App (Var var) Nothing [y]
            | C_prefix <- v_categ var
            -> var `Set.member` coercePrefixOps
                || (var `Set.member` simplePrefixOps && isSIMPLE y)
            | C_postfix <- v_categ var
            -> var `Set.member` simplePostfixOps && isSIMPLE y
        App (Var var) (Just y) []
            | C_prefix <- v_categ var
            -> var `Set.member` coercePrefixOps
                || (var `Set.member` simplePrefixOps && isSIMPLE y)
            | C_postfix <- v_categ var
            -> var `Set.member` simplePostfixOps && isSIMPLE y
        App (Var var) Nothing [x, y]
            | C_infix <- v_categ var
            -> var `Set.member` simpleInfixOps && isSIMPLE x && isSIMPLE y
        App (Var var) (Just x) [y]
            | C_infix <- v_categ var
            -> var `Set.member` simpleInfixOps && isSIMPLE x && isSIMPLE y
        _               -> isScalarLValue x

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
mergeStmts (Sym scope name x) y = Sym scope name (mergeStmts x y)
mergeStmts (Pad scope lex x) y = Pad scope lex (mergeStmts x y)
mergeStmts (Syn "package" [kind, pkg@(Val (VStr _))]) y =
    Syn "namespace" [kind, pkg, y]
mergeStmts x@(Ann ann (Syn syn _)) y | (syn ==) `any` words "subst match //"  =
    mergeStmts (Ann ann (App (_Var "&infix:~~") Nothing [_Var "$_", x])) y
mergeStmts x y@(Ann ann (Syn syn _)) | (syn ==) `any` words "subst match //"  =
    mergeStmts x (Ann ann (App (_Var "&infix:~~") Nothing [_Var "$_", y]))
mergeStmts (Ann ann (Syn "sub" [Val (VCode sub)])) y | subType sub == SubBlock =
    -- bare Block in statement level; annul all its parameters and run it!
    mergeStmts (Ann ann $ subBody sub) y
mergeStmts x (Ann ann (Syn "sub" [Val (VCode sub)])) | subType sub == SubBlock =
    -- bare Block in statement level; annul all its parameters and run it!
    mergeStmts x (Ann ann $ subBody sub)
mergeStmts x (Stmts y Noop) = mergeStmts x y
mergeStmts x (Stmts Noop y) = mergeStmts x y
mergeStmts x y = Stmts x y

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
    metaObj = _Sym SGlobal (':':'*':name) $! Syn ":="
        [ _Var (':':'*':name)
        , App (_Var "&META::new")
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
            ]
        ]

newType :: String -> Exp
newType name = _Sym SGlobal ('&':'&':'*':name) $! Syn ":="
    [ _Var ('&':'*':name)
    , typeMacro name (Val . VType . mkType $ name)
    ]

newMetaType :: String -> Exp
newMetaType name = _Sym SGlobal ('&':'&':'*':name) $! Syn ":="
    [ _Var ('&':'*':name)
    , typeMacro name (_Var (':':'*':name))
    ]

typeMacro :: String -> Exp -> Exp
typeMacro name exp = Syn "sub" . (:[]) . Val . VCode $ MkCode
    { isMulti       = True
    , subName       = cast ('&':name)
    , subEnv        = Nothing
    , subType       = SubMacro
    , subAssoc      = ANil
    , subReturns    = typ
    , subLValue     = False
    , subParams     = []
    , subBindings   = []
    , subSlurpLimit = []
    , subBody       = Prim . const . expToEvalVal $ exp
    , subCont       = Nothing
    }
    where
    typ = mkType name

{- utilities for filtering out primitives from an environmet, useful for
 - CodeGen and Pugs::Internals::emit_yaml -}

filterPrim :: (TVar Pad) -> Eval Pad
filterPrim glob = do
    MkPad pad   <- liftSTM $ readTVar glob
    fmap (MkPad . Map.fromAscList . catMaybes) . mapM checkPrim $ Map.toAscList pad

checkPrim :: (Var, PadEntry) -> Eval (Maybe (Var, PadEntry))
checkPrim e@(var, entry)
    | SType <- v_sigil var, isGlobalVar var = return Nothing
    | MkEntry (_, tv) <- entry = do
        rv <- isPrim tv
        return $ if rv then Nothing else Just e
    | otherwise = do
        let MkEntryMulti xs = entry
        xs' <- filterM (fmap not . isPrim . snd) xs
        return $ if null xs' then Nothing else Just (var, MkEntryMulti xs')

isPrim :: TVar VRef -> Eval Bool
isPrim tv = do
    vref <- liftSTM $ readTVar tv
    case vref of
        MkRef (ICode cv)    -> fmap (isPrimVal . VCode) (code_fetch cv)
        MkRef (IScalar sv)  -> fmap isPrimVal (scalar_fetch sv)
        _                   -> return False
    where
    isPrimVal (VCode MkCode{ subBody = Prim _ }) = True
    isPrimVal _ = False

filterUserDefinedPad :: Pad -> Pad
filterUserDefinedPad (MkPad pad) = MkPad $ Map.filterWithKey doFilter pad
    where
    doFilter key _ = (not . Set.member key) _reserved

_reserved :: Set Var
_reserved = Set.fromList . cast . words $
    "@*ARGS @*INC %*INC $*PUGS_HAS_HSPLUGINS $*EXECUTABLE_NAME " ++
    "$*PROGRAM_NAME $*PID $*UID $*EUID $*GID $*EGID @*CHECK @*INIT $*IN " ++
    "$*OUT $*ERR $*ARGS $/ %*ENV $*CWD @=POD $=POD $?PUGS_VERSION " ++
    "$*OS &?BLOCK_EXIT %?CONFIG $*_ $*AUTOLOAD"
