{-# OPTIONS -fglasgow-exts #-}

{-
    Runtime engine.

    The mountain throne once more is freed!
    O! Wandering folk, the summons heed!
    Come haste! Come haste! Across the waste!
    The king of friend and kin has need...
-}

module Run where
import Internals
import Config 
import AST
import Eval
import Prim

runEval :: Env -> Eval Val -> IO Val
runEval env eval = do
    my_perl <- initPerl5 ""
    val <- (`runReaderT` env) $ do
        (`runContT` return) $
            resetT eval
    freePerl5 my_perl
    return val

runEnv env = runEval env $ evaluateMain (envBody env)

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

prepareEnv name args = do
    environ <- getEnvironment
    let envFM = listToFM $ [ (k, VStr v) | (k, v) <- environ ]
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
        , SymVal SGlobal "%*ENV" (VHash . MkHash $ envFM)
        -- XXX What would this even do?
        -- , SymVal SGlobal "%=POD"        (Val . VHash . MkHash $ emptyFM) 
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
        ]


        
getLibs :: [(String, String)] -> IO [String]
getLibs environ = do
        args <- getArgs
        return $ filter (not . null) (libs args)
    where
    envlibs nm = maybe [] (split (getConfig "path_sep")) $ nm `lookup` environ
    inclibs args = map (drop 2) (filter isLibArg args)
    isLibArg ('-':'I':_) = True
    isLibArg _ = False
    libs args =  (inclibs args)
              ++ envlibs "PERL6LIB"
              ++ [ getConfig "archlib"
                 , getConfig "privlib"
                 , getConfig "sitearch"
                 , getConfig "sitelib"
                 ]
              ++ [ "." ]

