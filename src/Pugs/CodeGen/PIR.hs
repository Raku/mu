{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances -fno-warn-orphans -funbox-strict-fields -cpp #-}
{-# OPTIONS_GHC -#include "../../UnicodeC.h" #-}

{-|
    This module provides 'genPIR', a function which compiles the current
    environment to PIR code.

    The general plan is to first compile the environment (subroutines,
    statements, etc.) to an abstract syntax tree ('PIL' -- Pugs Intermediate
    Language) using the 'compile' function and 'Compile' class, and then
    translate the PIL_to a data structure of type 'PIR' using the 'trans'
    function and 'Translate' class. This data structure is then reduced to
    final PIR code by "Emit.PIR".
-}

module Pugs.CodeGen.PIR (genPIR) where
import Pugs.Internals
import Pugs.AST
import Pugs.Types
import Pugs.Eval.Var
import Pugs.PIL1
import Emit.PIR
import Pugs.Pretty
import Text.PrettyPrint
import Pugs.CodeGen.PIR.Prelude (preludeStr)
import Pugs.Prim.Eval
import Pugs.Compile
import Pugs.Run (getLibs)

type CodeGen a = WriterT [Stmt] (ReaderT TEnv IO) a
type CodeGenMonad = WriterT [Stmt] (ReaderT TEnv IO)

ratToNum :: VRat -> VNum
ratToNum x = (fromIntegral $ numerator x) / (fromIntegral $ denominator x)

{-| Currently only 'PIL' → 'PIR' -}
class (Show a, Typeable b) => Translate a b | a -> b where
    trans :: a -> CodeGen b
    trans _ = fail "Untranslatable construct!"

instance EnterClass CodeGenMonad TCxt where
    enter cxt = local (\e -> e{ tCxt = cxt })

transError :: forall a b. Translate a b => a -> CodeGen b
transError = die $ "Translate error -- invalid "
    ++ (show $ typeOf (undefined :: b))

instance Translate PIL_Stmts [Stmt] where
    trans PNil = return []
    trans (PStmts this rest) = do
        thisC   <- trans this
        tell [thisC]
        trans rest
    trans (PPad SMy pad exps) = do
        valsC   <- mapM trans (map snd pad)
        pass $ do
            expsC   <- trans exps
            return ([], (StmtPad (map fst pad `zip` valsC) expsC:))
    trans (PPad _ pad exps) = do
        -- XXX - maybe warn about bad pads?
        trans (PPad SMy pad exps)

instance Translate PIL_Stmt Stmt where
    trans PNoop = return (StmtComment "")
    trans (PStmt (PLit (PVal VUndef))) = return $ StmtComment ""
    trans (PStmt exp) = do
        expC    <- trans exp
        return $ StmtIns $ InsExp expC
    trans (PPos pos exp rest) = do
        dep     <- asks tTokDepth
        tell [StmtComment $ (replicate dep ' ') ++ "{{{ " ++ pretty exp]
        expC    <- local (\e -> e{ tTokDepth = dep + 1 }) $ trans rest
        tell [StmtComment $ (replicate dep ' ') ++ "}}} " ++ pretty pos]
        return expC

instance Translate PIL_Expr Expression where
    trans (PRawName name) = fmap ExpLV $ genName name
    trans (PExp exp) = fmap ExpLV $ trans exp
    trans (PLit (PVal VUndef)) = do
        pmc     <- genLV "undef"
        return $ ExpLV pmc
    trans (PLit lit) = do
        -- generate fresh supply and things...
        litC    <- trans lit
        pmc     <- genLV "lit"
        tellIns $ pmc <== ExpLit litC
        return $ ExpLV pmc
    trans (PThunk exp) = do
        [begL, sndL, retL, endL] <- genLabel ["thunkBegin", "thunkAgain", "thunkReturn", "thunkEnd"]
        this    <- genPMC "block"
        tellIns $ InsNew (reg this) Continuation
        tellIns $ "set_addr" .- [reg this, bare begL]
        tellIns $ "goto" .- [bare endL]
        tellLabel begL
        cc      <- genPMC "cc"
        fetchCC cc (reg this)
        expC    <- trans exp
        tellIns $ "set_addr" .- [reg this, bare sndL]
        tellIns $ "goto" .- [bare retL]
        tellLabel sndL
        fetchCC cc (reg this)
        tellLabel retL
        tellIns $ if parrotBrokenXXX
            then "store_global" .- [tempSTR, expC]
            else "set_args" .- [lit "(0b10)", expC]
        tellIns $ "invokecc" .- [reg cc]
        tellLabel endL
        return (ExpLV this)
    trans (PCode styp params _ _ body) = do
        [begL, endL] <- genLabel ["blockBegin", "blockEnd"]
        this    <- genPMC "block"
        tellIns $ InsNew (reg this) Closure
        tellIns $ "set_addr" .- [reg this, bare begL]
        tellIns $ "goto" .- [bare endL]
        tellLabel begL
        let prms = map tpParam params
        mapM_ (tellIns . InsLocal RegPMC . prmToIdent) prms
        tellIns $ "get_params" .- sigList (map prmToSig prms)
        tellIns $ "new_pad" .- [lit curPad]
        wrapSub styp $ do
            mapM storeLex params
            trans body  -- XXX - consistency check
            bodyC   <- lastPMC
            tellIns $ "set_returns" .- retSigList [bodyC]
            tellIns $ "returncc" .- []
        tellLabel endL
        return (ExpLV this)

instance Translate PIL_Decl Decl where
    trans (PSub name styp params lvalue ismulti body) | Just (pkg, name') <- isQualified name = do
        declC <- trans $ PSub name' styp params lvalue ismulti body
        return $ DeclNS pkg [declC]
    trans (PSub name styp params _ _ body) = do
        (_, stmts)  <- listen $ do
            let prms = map tpParam params
            mapM_ (tellIns . InsLocal RegPMC . prmToIdent) prms
            tellIns $ "get_params" .- sigList (map prmToSig prms)
            tellIns $ "new_pad" .- [lit curPad]
            wrapSub styp $ do
                mapM storeLex params
                trans body
                bodyC <- lastPMC
                tellIns $ "set_returns" .- retSigList [bodyC]
                tellIns $ "returncc" .- []
        return (DeclSub name [] stmts)

instance Translate PIL_Literal Literal where
    trans (PVal (VBool bool)) = return $ LitInt (toInteger $ fromEnum bool)
    trans (PVal (VStr str)) = return $ LitStr str
    trans (PVal (VInt int)) = return $ LitInt int
    trans (PVal (VNum num)) = return $ LitNum num
    trans (PVal (VRat rat)) = return $ LitNum (ratToNum rat)
    trans (PVal (VList [])) = return $ LitInt 0 -- XXX Wrong
    trans val@(PVal _) = transError val

instance Translate PIL_LValue LValue where
    trans (PVar name) | Just (pkg, name') <- isQualified name = do
        -- XXX - this is terribly ugly.  Fix at parrot side perhaps?
        pmc     <- genLV "glob"
        let initL   = "init_" ++ pmcStr
            doneL   = "done_" ++ pmcStr
            pmcStr  = render (emit pmc)
        tellIns $ "push_eh" .- [bare initL]
        tellIns $ pmc <-- "find_global" $ [lit pkg, lit name']
        tellIns $ "goto" .- [bare doneL]
        tellLabel initL
        tellIns $ "store_global" .- [lit pkg, lit name', reg pmc]
        tellLabel doneL
        tellIns $ "clear_eh" .- []
        return pmc
    trans (PVar name) = do
        pmc     <- genLV "lex"
        tellIns $ pmc <-- "find_name" $ [lit $ possiblyFixOperatorName name]
        return pmc
    trans (PAssign [lhs] rhs) = do
        lhsC    <- enter tcLValue $ trans lhs
        rhsC    <- trans rhs
        tellIns $ lhsC <== rhsC
        return lhsC
    trans (PBind [PVar name] rhs)
        | Just (pkg, name') <- isQualified (qualify name) = do
        rhsC    <- trans rhs
        tellIns $ "store_global" .- [lit pkg, lit name', rhsC]
        trans (PVar name)
    trans (PBind [lhs] rhs) = do
        lhsC    <- enter tcLValue $ trans lhs
        rhsC    <- trans rhs
        tellIns $ lhsC <:= rhsC
        return lhsC
    trans (PApp _ exp@(PCode _ _ _ _ _) Nothing []) = do
        blockC  <- trans exp
        tellIns $ [reg tempPMC] <-& blockC $ []
        return tempPMC
    trans (PApp (TCxtLValue _) (PExp (PVar "&postcircumfix:[]")) Nothing [PExp lhs, rhs]) = do
        lhsC    <- trans lhs
        rhsC    <- trans rhs
        return $ lhsC `KEYED` rhsC
    trans (PApp ctx fun (Just inv) args) =
        trans (PApp ctx fun Nothing (inv:args))  -- XXX wrong
    trans (PApp _ fun Nothing args) = do
        funC    <- trans fun {- case fun of
            PExp (PVar name) -> return $ lit name
            _           -> trans fun
        -}
        argsC   <- mapM trans args
        -- XXX WORKAROUND PARROT BUG (see below)
        pmc     <- genLV "app"
        -- XXX - probe if funC is slurpy, then modify ExpLV pmc accordingly
        tellIns $ [reg pmc] <-& funC $ argsC
        return pmc
        {- XXX PARROT BUG -- tailcall broken
        case cxt of
            TTailCall _ -> do
                tellIns $ InsTailFun funC argsC
                return nullPMC
            _ -> do
                pmc     <- genLV "app"
                -- XXX - probe if funC is slurpy, then modify ExpLV pmc accordingly
                tellIns $ [reg pmc] <-& funC $ argsC
                return pmc
        -}
    trans x = transError x

fetchCC :: LValue -> Expression -> CodeGen ()
fetchCC cc begL | parrotBrokenXXX = do
    tellIns $ tempINT   <-- "get_addr" $ [begL]
    tellIns $ tempSTR   <:= tempINT
    tellIns $ "find_global" .- [reg cc, tempSTR]
fetchCC cc _ = do
    tellIns $ "get_params" .- sigList [reg cc]

-- XXX - slow way of implementing "return"
wrapSub :: SubType -> CodeGen () -> CodeGen ()
wrapSub SubPrim = id
wrapSub SubBlock = id -- XXX not really
wrapSub _ = \body -> do
    [retL, errL] <- genLabel ["returnHandler", "errHandler"]
    tellIns $ "push_eh" .- [bare retL]
    body
    tellLabel retL
    tellIns $ tempPMC <:= ExpLV (errPMC `KEYED` lit False)
    tellIns $ "clear_eh" .- []
    tellIns $ tempSTR <-- "typeof" $ [errPMC]
    tellIns $ "eq" .- [tempSTR, lit "Exception", bare errL]
    tellIns $ "set_returns" .- sigList [tempPMC]
    tellIns $ "returncc" .- []
    tellLabel errL
    tellIns $ "throw" .- [errPMC]

prmToSig :: Param -> Sig
prmToSig prm = MkSig (prmToArgs prm) . bare $ prmToIdent prm

prmToArgs :: Param -> [ArgFlag]
prmToArgs prm = combine 
    [ isSlurpy   ==> MkArgSlurpyArray
    , isOptional ==> MkArgOptional
    ] []
    where
    f ==> arg = if f prm then (arg:) else id

prmToIdent :: Param -> String
prmToIdent = render . varText . paramName

storeLex :: TParam -> CodeGen ()
storeLex param = do
    when (isOptional prm) $ do
        [defC] <- genLabel ["defaultDone"]
        tellIns $ "unless_null" .- [bare name, bare defC]
        case tpDefault param of
            Nothing     -> tellIns $ InsNew (VAR name) PerlScalar
            (Just exp)  -> do
                expC <- trans exp
                -- compile it away
                tellIns $ VAR name <:= expC
        tellLabel defC
    tellIns $ "store_lex" .- [lit curPad, lit var, bare name]
    where
    var     = paramName prm
    name    = prmToIdent prm
    prm     = tpParam param

tellIns :: Ins -> CodeGen ()
tellIns = tell . (:[]) . StmtIns

{-| Inserts a label. -}
tellLabel :: String -> CodeGen ()
tellLabel name = tellIns $ InsLabel name

lastPMC :: (RegClass a) => CodeGen a
lastPMC = do
    tvar    <- asks tReg
    name'   <- liftIO $ liftSTM $ do
        (cur, name) <- readTVar tvar
        return $ ('P':show cur) ++ (if null name then name else ('_':name))
    return $ reg (VAR name')

genPMC :: (RegClass a) => String -> CodeGen a
genPMC name = do
    tvar    <- asks tReg
    name'   <- liftIO $ liftSTM $ do
        (cur, _) <- readTVar tvar
        writeTVar tvar (cur + 1, name)
        return $ ('P':show (cur + 1)) ++ ('_':name)
    tellIns $ InsLocal RegPMC name'
    return $ reg (VAR name')

genLV :: (RegClass a) => String -> CodeGen a
genLV name = do
    pmc <- genPMC name
    tellIns $ InsNew pmc PerlScalar
    return $ reg pmc

genLabel :: [String] -> CodeGen [LabelName]
genLabel names = do
    tvar    <- asks tLabel
    cnt     <- liftIO $ liftSTM $ do
        cur <- readTVar tvar
        writeTVar tvar (cur + 1)
        return cur
    return $ map (\name -> "LABEL_" ++ show cnt ++ ('_':name)) names

genName :: (RegClass a) => String -> CodeGen a
genName name = do
    let var = render $ varText name
    tellIns $ InsLocal RegPMC var
    tellIns $ InsNew (VAR var) (varInit name)
    return $ reg (VAR var)

varInit :: String -> ObjType
varInit ('$':_) = PerlScalar
varInit ('@':_) = PerlArray
varInit ('%':_) = PerlHash
varInit ('&':_) = PerlScalar
varInit x       = internalError $ "Invalid name: " ++ x

{-| Compiles the current environment to PIR code. -}
genPIR :: Eval Val
genPIR = do
    tenv        <- initTEnv
    -- Load the PIR Prelude.
    local (\env -> env{ envDebug = Nothing }) $ do
        opEval style "<prelude-pir>" preludeStr
    penv        <- compile ()
    globPIR     <- runCodeGenGlob tenv (pilGlob penv)
    mainPIR     <- runCodeGenMain tenv (pilMain penv)
    libs        <- liftIO $ getLibs
    return . VStr . unlines $
        [ "#!/usr/bin/env parrot"
        , renderStyle (Style PageMode 0 0) $ preludePIR $+$ vcat
        -- Namespaces have bugs in both pugs and parrot.
        [ emit globPIR
        , emit $ DeclNS "main"
        [ DeclSub "init" [SubMAIN, SubANON] $ map StmtIns (
            -- Eventually, we'll have to write our own find_name wrapper (or
            -- fix Parrot's find_name appropriately). See Pugs.Eval.Var.
            -- For now, we simply store $P0 twice.
            [ "new_pad" .- [lit0]
            , InsNew tempPMC PerlEnv
            , "store_global"    .- [lit "%*ENV", tempPMC]
            , "store_global"    .- [lit "%ENV", tempPMC]
            , InsNew tempPMC PerlArray
            ] ++ [ "push" .- [tempPMC, lit path] | path <- libs ] ++
            [ "store_global"    .- [lit "@*INC", tempPMC]
            , "store_global"    .- [lit "@INC", tempPMC]
            , InsNew tempPMC PerlArray
            , "store_global"    .- [lit "@*END", tempPMC]
            , "store_global"    .- [lit "@END", tempPMC]
            , "getstdin"        .- [tempPMC]
            , "store_global"    .- [lit "$*IN", tempPMC]
            , "store_global"    .- [lit "$IN", tempPMC]
            , "getstdout"       .- [tempPMC]
            , "store_global"    .- [lit "$*OUT", tempPMC]
            , "store_global"    .- [lit "$OUT", tempPMC]
            , "getstderr"       .- [tempPMC]
            , "store_global"    .- [lit "$*ERR", tempPMC]
            , "store_global"    .- [lit "$ERR", tempPMC]
            , "getinterp"       .- [tempPMC]
            , tempPMC   <:= ExpLV (tempPMC `KEYED` bare ".IGLOBALS_ARGV_LIST")
            , tempPMC2  <-- "shift" $ [tempPMC]
            , "store_global"    .- [lit "@*ARGS", tempPMC]
            , "store_global"    .- [lit "@ARGS", tempPMC]
            , "store_global"    .- [lit "$*PROGRAM_NAME", tempPMC2]
            , "store_global"    .- [lit "$PROGRAM_NAME", tempPMC2]
            -- XXX wrong, should be lexical
            , InsNew tempPMC PerlScalar
            , "store_global"    .- [lit "$_", tempPMC]
            ]) ++ [ StmtRaw (text (name ++ "()")) | PSub name@('_':'_':_) _ _ _ _ _ <- pilGlob penv ] ++
            [ StmtRaw (text "main()")
            , StmtIns $ tempPMC  <-- "find_global" $ [lit "Perl6::Internals", lit "&exit"]
            , StmtIns $ "set_args" .- sigList [MkSig [] lit0]
            , StmtIns $ "invokecc" .- [tempPMC]
            ]
        , DeclSub "main" [SubANON] [ StmtRaw $ emit mainPIR ]
        ] ] ]
    where
    style = MkEvalStyle
        { evalResult = EvalResultModule
        , evalError  = EvalErrorFatal
        }

runCodeGenGlob :: TEnv -> [PIL_Decl] -> Eval [Decl]
runCodeGenGlob tenv = mapM $ fmap fst . runCodeGen tenv

runCodeGenMain :: TEnv -> PIL_Stmts -> Eval [Stmt]
runCodeGenMain tenv = fmap snd . runCodeGen tenv

runCodeGen :: (Translate a b) => TEnv -> a -> Eval (b, [Stmt])
runCodeGen tenv = liftIO . (`runReaderT` tenv) . runWriterT . trans
