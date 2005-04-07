{-# OPTIONS_GHC -fglasgow-exts #-}

{-
    Runtime engine.

    The mountain throne once more is freed!
    O! Wandering folk, the summons heed!
    Come haste! Come haste! Across the waste!
    The king of friend and kin has need...
-}

module Run where
import Run.Args
import Internals
import Config
import AST
import Eval
import Prim
import qualified Data.Map as Map

runWithArgs f = do
    args <- getArgs
    f $ canonicalArgs args

runEval :: Env -> Eval Val -> IO Val
runEval env eval = withSocketsDo $ do
    my_perl <- initPerl5 ""
    val <- (`runReaderT` env) $ do
        (`runContT` return) $
            resetT eval
    freePerl5 my_perl
    return val

runEnv :: Env -> IO Val
runEnv env = runEval env $ evaluateMain (envBody env)

runAST :: Exp -> IO Val
runAST ast = do
    hSetBuffering stdout NoBuffering
    name <- getProgName
    args <- getArgs
    env  <- prepareEnv name args
    runEnv env{ envBody = ast, envDebug = Nothing }

runComp :: Eval Val -> IO Val
runComp comp = do
    hSetBuffering stdout NoBuffering
    name <- getProgName
    args <- getArgs
    env  <- prepareEnv name args
    runEval env{ envDebug = Nothing } comp

prepareEnv :: VStr -> [VStr] -> IO Env
prepareEnv name args = do
    environ <- getEnvironment
    let envHV = Map.fromList $ [ (k, VStr v) | (k, v) <- environ ]
    let confHV = Map.fromList $ [ (k, VStr v) | (k, v) <- Map.toList config ]
    exec    <- getArg0
    libs    <- getLibs environ
    execSV  <- newMVal $ VStr exec
    progSV  <- newMVal $ VStr name
    endAV   <- newMVal $ VList []
    matchAV <- newMVal $ VList []
    incAV   <- newMVal $ VList (map VStr libs)
    argsAV  <- newMVal $ VList (map VStr args)
    inGV    <- newMVal $ VHandle stdin
    outGV   <- newMVal $ VHandle stdout
    errGV   <- newMVal $ VHandle stderr
    errSV   <- newMVal $ VStr ""
    defSV   <- newMVal $ VUndef
    let subExit = \x -> case x of
            [x] -> op1 "exit" x
            _   -> op1 "exit" VUndef
    emptyEnv
        [ SymVal SGlobal "@*ARGS"       argsAV
        , SymVal SGlobal "@*INC"        incAV
        , SymVal SGlobal "$*EXECUTABLE_NAME"    execSV
        , SymVal SGlobal "$*PROGRAM_NAME"       progSV
        , SymVal SGlobal "@*END"        endAV
        , SymVal SGlobal "$*IN"         inGV
        , SymVal SGlobal "$*OUT"        outGV
        , SymVal SGlobal "$*ERR"        errGV
        , SymVal SGlobal "$!"           errSV
        , SymVal SGlobal "$/"           matchAV
        , SymVal SGlobal "%*ENV" (VHash envHV)
        -- XXX What would this even do?
        -- , SymVal SGlobal "%=POD"        (Val . VHash $ emptyHV)
        , SymVal SGlobal "@=POD"        (VArray . MkArray $ [])
        , SymVal SGlobal "$=POD"        (VStr "")
        , SymVal SGlobal "$?OS"         (VStr (getConfig "osname"))
        , SymVal SGlobal "$?_BLOCK_EXIT" $ VSub $ Sub
            { isMulti = False
            , subName = "$?_BLOCK_EXIT"
            , subType = SubPrim
            , subPad = []
            , subAssoc = "pre"
            , subParams = []
            , subBindings = []
            , subReturns = "Void"
            , subFun = Prim subExit
            }
        , SymVal SGlobal "%?CONFIG" (VHash confHV)
        , SymVal SMy "$_" defSV
        ]



getLibs :: [(String, String)] -> IO [String]
getLibs environ = do
        args <- getArgs
        return $ filter (not . null) (libs (canonicalArgs args))
    where
    envlibs nm = maybe [] (split (getConfig "path_sep")) $ nm `lookup` environ

    -- broken, need real parser
    inclibs ("-I":dir:rest) = [dir] ++ inclibs(rest)
    inclibs (_:rest)        = inclibs(rest)
    inclibs ([])            = []

    libs args =  (inclibs args)
              ++ envlibs "PERL6LIB"
              ++ [ getConfig "archlib"
                 , getConfig "privlib"
                 , getConfig "sitearch"
                 , getConfig "sitelib"
                 ]
              ++ [ "." ]

