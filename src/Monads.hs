{-# OPTIONS -fglasgow-exts #-}

{-
    Monad structures.

    One Ring to rule them all,
    One Ring to find them,
    One Ring to bring them all
    and in the darkness bind them...
-}

module Monads where
import Internals
import AST


{-

type Exp = ()
type Cxt = String
type Symbols = [String]
type ClassTree = [String]
data Val = VStr String
         | VErr VErr
         deriving (Show, Eq)

instance Show (a -> b) where
    show f = "sub { ... }"
instance Eq (Env -> Bool) where
    _ == _ = False


data Env = Env { envContext :: Cxt
               , envPad     :: Symbols
               , envCaller  :: Maybe Env
               , envClasses :: ClassTree
               , envEval    :: Eval Val
               , envCC      :: Val -> Eval Val
               , envBody    :: Exp
               , envDepth   :: Int
               , envID      :: Unique
               }

env = Env { envContext = "List"
          , envPad = []
          , envCaller = Nothing
          , envClasses = []
          , envEval = undefined
          , envCC = return
          , envBody = ()
          , envDepth = 0
          , envID = undefined
          }

{-
eval exp = do
    local (\e -> e{ envBody = exp }) $ do
        return
-}

askDump str = do
    env <- asks envContext
    liftIO $ putStrLn $ "Current scope: " ++ str ++ " - Env: " ++ env

main = do
    uniq <- newUnique
    x <- (`runReaderT` env { envID = uniq }) $ do
        y <- (`runContT` return) $ blah
        return y
    print x
    return x

data VSub = Sub
    { subName       :: String
    , subPad        :: Symbols
    , subFun        :: Eval Val
    }

enterScope f = do
    uniq <- liftIO $ newUnique
    rv <- callCC $ \cc -> resetT $ do
        local (\e -> e{ envCaller = Just e, envCC = cc, envDepth = 1 + envDepth e, envID = uniq } ) f
    liftIO $ print (rv)
    -- here we trigger error handler of various sorts
    case rv of
        VErr (ErrRet f val) -> do
            env <- ask
            if f env
                then callerReturn 0 val
                else return rv
        _ -> return rv
    {-
    -- detect for abnormal return
    return rv
    -}

enterSub sub = enterScope $ do
    local (\e -> e { envPad = subPad sub }) $ do
        subFun sub

innerSub = Sub "inner" ["$inner"] inner

-- enter a lexical context
enterLex str = local (\e -> e { envPad = (str:envPad e) })

dumpLex :: String -> Eval ()
dumpLex label = do
    pad <- asks envPad
    depth <- asks envDepth
    liftIO $ putStrLn ("("++(show depth)++")"++label ++ ": " ++ (show pad))
    return ()

blah :: Eval Val
blah = do
    dumpLex ">init"
    rv <- enterLex "$x" $ do
        dumpLex ">lex"
        rv <- enterScope outer
        dumpLex "<lex"
        return rv
    dumpLex "<init"
    return rv

outer :: Eval Val
outer = enterLex "$outer" $ do
    dumpLex ">outer"
    enterSub innerSub
    dumpLex "<outer"
    returnScope "y"
    returnScope "c"

callerCC :: Int -> Val -> Eval Val
callerCC n v = do
    env <- caller n
    (envCC env) v

caller :: Int -> Eval Env
caller n = do
    depth <- asks envDepth
    when (depth <= n) $
        fail "Cannot ask for deeper depth"
    asks $ foldl (.) id $ replicate n (fromJust . envCaller)

inner :: Eval Val
inner = do
    dumpLex ">inner"
    -- now try raising exceptions via multiple delimiting
    -- fail "foo"
    callerReturn 1 (VStr "happy")
    -- returnScope "foo"
    -- env <- caller 2
    -- throwErr $ ErrStr "test"
    -- (envShift env) $ \r -> return $ VStr "out1"

{-
throwErr :: (MonadIO m) => VErr -> m a
throwErr = liftIO . throwIO . DynException . toDyn
-}

callerReturn :: Int -> Val -> Eval Val
callerReturn n v
    | n == 0 =  do
        shiftT $ \r -> return v
    | otherwise = do
        env <- caller n
        shiftT $ \r -> return $ VErr $ ErrRet ((==) (envID env) . envID) v

returnScope = callerReturn 0 . VStr

data VErr = ErrStr String
          | ErrRet (Env -> Bool) Val
    deriving (Typeable, Show, Eq)


-}
