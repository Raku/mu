{-# OPTIONS_GHC -fglasgow-exts -fallow-overlapping-instances #-}
module Pugs.Prim.Eval (
    -- used by Pugs.Prim
    op1EvalHaskell, op1EvalP6Y, op1EvalFileP6Y,
    opEval, opEvalFile,
    opRequire, requireInc,
    EvalError(..), EvalResult(..), EvalStyle(..),
    -- used by Pugs.Eval -- needs factored somewhere bettwen
    retEvalResult,
) where
import Pugs.AST
import Pugs.Parser.Program
import Pugs.Embed
import Pugs.Monads
import Pugs.Internals
import Pugs.Pretty
import Pugs.Config
import Pugs.Prim.Keyed
import Pugs.Types
import Pugs.Prelude
import DrIFT.YAML
import Data.Yaml.Syck
import Data.Binary (decode)
import qualified Data.ByteString.Char8 as Bytes

type Bytes        = Bytes.ByteString

data EvalError = EvalErrorFatal
               | EvalErrorUndef
               deriving Eq
data EvalResult = EvalResultLastValue
                | EvalResultModule
                | EvalResultEnv
                deriving Eq
data EvalStyle = MkEvalStyle
               { evalError  :: EvalError
               , evalResult :: EvalResult
               }


specialPackageNames :: [String]
specialPackageNames = ["MY", "OUR", "GLOBAL", "OUTER", "CALLER", "ENV", "SUPER", "COMPILING"]

opRequire :: Bool -> Val -> Eval Val
opRequire dumpEnv v = do
    mod         <- fromVal v
    if elem mod specialPackageNames then return (VBool True) else do
    incs        <- fromVal =<< readVar (cast "@*INC")
    glob        <- askGlobal
    seen        <- findSymRef (cast "%*INC") glob
    loaded      <- existsFromRef seen v
    let file | '.' `elem` mod = mod
             | otherwise      = (concat $ intersperse (getConfig "file_sep") $ split "::" mod) ++ ".pm"
    pathName    <- case mod of
        "Test"  -> return "Test.pm"
        _       -> requireInc incs file (errMsg file incs)
    if loaded then opEval style pathName "" else do
        -- %*INC{mod} = { relname => file, pathname => pathName }
        evalExp $ Syn "="
            [ Syn "{}"             -- subscript
                [ _Var "%*INC", Val . VStr $ decodeUTF8 mod ]
                , Syn "\\{}"       -- hashref
                    [ Syn "," [ mkStrPair "fullpath" (decodeUTF8 pathName)
                              , mkStrPair "relpath"  (decodeUTF8 file) ]
                    ]
            ]
        -- merge @*END here
        endAV   <- findSymRef (cast "@*END") glob
        ends    <- fromVal =<< readRef endAV
        clearRef endAV
        rv <- case mod of
            "Test"  -> shortcutToTestPM
            _       -> tryFastEval pathName (pathName ++ ".yml")
        endAV'  <- findSymRef (cast "@*END") glob
        doArray (VRef endAV') (`array_unshift` ends)
        return rv
    where
    shortcutToTestPM = do
        globTVar    <- asks envGlobal
        let MkCompUnit _ _ glob ast = decode (testByteStringLazy)
        -- Inject the global bindings
        stm $ do
            glob' <- readMPad globTVar
            writeMPad globTVar (glob `unionPads` glob')

        -- | PEStatic   { pe_type :: !Type, pe_proto :: !VRef, pe_flags :: !EntryFlags, pe_store :: !(TVar VRef) }
        evl <- asks envEval
        evl ast
    tryFastEval pathName pathNameYml = do
        io $ print pathNameYml
        ok <- io $ doesFileExist pathNameYml
        if not ok then slowEval pathName else do
        isYamlStale <- tryIO False $ do
            timePm  <- getModificationTime pathName
            timeYml <- getModificationTime pathNameYml
            return (timeYml < timePm)
        if isYamlStale then slowEval pathName else do
        rv <- tryT $ fastEval pathNameYml
        case rv of
            VError _ [MkPos{posBeginLine=0}]-> slowEval pathName
            _                               -> opEval style pathName ""
        
        
    fastEval = op1EvalFileP6Y . VStr
    slowEval pathName = do 
        str      <- io $ readFile pathName
        opEval style pathName str
    style = MkEvalStyle
        { evalError  = EvalErrorFatal
        , evalResult = (if dumpEnv == True then EvalResultEnv
                                           else EvalResultLastValue)}
    errMsg file incs = "Can't locate " ++ file ++ " in @*INC (@*INC contains: " ++ unwords incs ++ ")."
    mkStrPair :: String -> String -> Exp
    mkStrPair key val = App (_Var "&infix:=>") Nothing (map (Val . VStr) [key, val])

requireInc :: (MonadIO m) => [FilePath] -> FilePath -> String -> m String
requireInc [] _ msg = fail msg
requireInc (p:ps) file msg = do
    let pathName  = p ++ (getConfig "file_sep") ++ file
    ok <- io $ doesFileExist pathName
    if (not ok)
        then requireInc ps file msg
        else return pathName

opEvalFile :: String -> Eval Val
opEvalFile filename = do
    ok <- io $ doesFileExist filename
    if (not ok)
        then fail $ "Can't locate " ++ filename ++ "."
        else do
            contents <- io $ readFile filename
            opEval style filename contents
    where
    style = MkEvalStyle{ evalError=EvalErrorUndef
                       , evalResult=EvalResultLastValue}

op1EvalHaskell :: Val -> Eval Val
op1EvalHaskell cv = do
    str     <- fromVal cv
    val     <- tryT $ evalHaskell str
    retEvalResult style val
    where
    style = MkEvalStyle{ evalError=EvalErrorUndef
                       , evalResult=EvalResultLastValue}


op1EvalP6Y, op1EvalFileP6Y :: Val -> Eval Val

op1EvalFileP6Y fileName = do
    fileName' <- fromVal fileName
    file      <- io $ Bytes.readFile fileName'
    op1EvalP6Y' file

op1EvalP6Y bytecode = do
    bytecode' <- fromVal bytecode
    op1EvalP6Y' $ Bytes.pack bytecode' -- XXX: is this the right pack function?

op1EvalP6Y' :: Bytes -> Eval Val
op1EvalP6Y' bytecode = do
    yml  <- io $ (`catchIO` (return . Left . show)) $
        fmap Right (parseYamlBytes bytecode)
    case yml of
        Right MkNode{ n_elem=ESeq (v:_) }
            | MkNode{ n_elem=EStr vnum } <- v
            , vnum /= (packBuf $ show compUnitVersion) -> do
                err $ "incompatible version number for compilation unit: found " ++
                    unpackBuf vnum ++ ", expecting " ++ (show compUnitVersion)
        Right yml' -> do
            globTVar    <- asks envGlobal
            MkCompUnit _ _ glob ast <- io $ fromYAML yml'
            tryT $ do
                -- Inject the global bindings
                stm $ do
                    glob' <- readMPad globTVar
                    writeMPad globTVar (glob `unionPads` glob')
                evl <- asks envEval
                evl ast
        x -> err x
    where
    err x = local (\e -> e{ envPos = (envPos e){ posBeginLine=0 } }) $
        fail $ "failed loading Yaml: " ++ show x

opEval :: EvalStyle -> FilePath -> String -> Eval Val
opEval style path str = enterCaller $ do
    env     <- ask
    let errHandler err = return env{ envBody = Val $ VError (VStr (show err)) [] }
    env'    <- io $ evaluateIO (parseProgram env path str) `catchIO` errHandler
    val     <- tryT $ local (const env') $ do
        evl <- asks envEval
        initAV   <- evalExp (_Var "@*INIT")
        initSubs <- fromVals initAV
        mapM_ evalExp [ Ann (Cxt CxtVoid) (App (Val sub) Nothing []) | sub@VCode{} <- initSubs ]
        evalExp (Syn "=" [_Var "@*INIT", Syn "," []])
        evl $ case evalResult style of
            EvalResultEnv   -> envBody env' `mergeStmts` Syn "continuation" []
            _               -> envBody env'
    retEvalResult style val

retEvalResult :: EvalStyle -> Val -> Eval Val
retEvalResult style val = do
    glob <- askGlobal
    errSV <- findSymRef (cast "$!") glob
    case val of
        err@(VError e _) -> do
            writeRef errSV e
            when (evalError style == EvalErrorFatal) $ do
                io $ fail $ pretty err
            retEmpty
        _ -> do
            writeRef errSV VUndef
            return val

