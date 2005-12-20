{-# OPTIONS_GHC -fglasgow-exts -fno-warn-orphans -funbox-strict-fields #-}

module PIL.Native.Eval (evalNativeLang, resumeNativeLang, EvalResult(..)) where
import PIL.Native.Prims
import PIL.Native.Types
import PIL.Native.Pretty
import PIL.Native.Coerce
import PIL.Native.Objects
import PIL.Native.Parser
import Data.FunctorM
import Data.Dynamic
import Data.Maybe
import Control.Monad.State
import Control.Monad.Reader
import Control.Exception

{-| 

PIL.Native.Eval

This is an evaluator for the core runtime mini-language.

See Also:

  PIL.Native.Parser
  PIL.Native.Prims  
  PIL.Native.Pretty  

-}

type Eval = StateT ObjectSpace (ReaderT Pad IO)
data EvalResult = MkEvalResult
    { result_value  :: !Native
    , result_pad    :: !Pad
    , result_objs   :: !ObjectSpace
    }
    deriving (Typeable)

instance MonadSTM Eval where
    liftSTM = lift . lift . liftSTM

evalNativeLang :: [NativeLangExpression] -> IO EvalResult
evalNativeLang = handleResult .  (`runReaderT` empty) . (`runStateT` empty) . evalMain

resumeNativeLang :: EvalResult -> [NativeLangExpression] -> IO EvalResult
resumeNativeLang res exps = do
    handleResult . (`runReaderT` result_pad res) .
                   (`runStateT` result_objs res) . evalExps $ exps

handleResult :: IO (Native, ObjectSpace) -> IO EvalResult
handleResult f = do
    (NError err, objs) <- f
    let (val, pad) = fromJust . fromDynamic . fromJust . dynExceptions $ err
    return $ MkEvalResult val pad objs

evalMain :: [NativeLangExpression] -> Eval Native
evalMain exps = bootstrapClass $ do
    -- addClassMethods
    evalExps exps

{-
addClassMethods :: Eval Native
addClassMethods = do
    add "has_method"        "-> $name { self.get_attr('%!methods').exists($name) }"
    add "get_method"        "-> $name { self.get_attr('%!methods').fetch($name) }"
    add "get_method_list"   "-> { self.get_attr('%!methods').keys() }"
    add "new"               "-> %prms { self.bless(nil, %prms) }"
    add "bless"           $ "-> $repr, %prms {                              \
                          \     -> $obj { $obj.BUILDALL(%prms); $obj; }     \
                          \         .(self.CREATE($repr, %prms))            \
                          \  }"
    add "CREATE"          $ "-> $repr, %prms {} " -- XXX - not finished yet
    where
    add name body = eval $ "::Class.add_method('" ++ name ++ "', " ++ body ++ ")"

eval :: String -> Eval Native
eval = evalExp . parseExp
-}

bootstrapClass :: Eval a -> Eval a
bootstrapClass x = mdo
    cls <- newObject cls $ mkMap
        [ ("@!MRO",              emptySeq)
        , ("@!subclasses",       emptySeq)
        , ("@!superclasses",     emptySeq)
        , ("%!private_methods",  emptyMap)
        , ("%!attributes",       emptyMap)
        , ("%!methods", toNative $ mkMap [("add_method", addMethod)])
        ]
    enterLex [("::Class", cls)] x
    where
    addMethod = parseSub
        "-> $name, &method { self.set_attr_hash('%!methods', $name, &method) }"

enterLex :: IsNative a => [(String, a)] -> Eval b -> Eval b
enterLex = local . append . mkMap . map (\(x, y) -> (x, toNative y))

evalExps :: [NativeLangExpression] -> Eval Native
evalExps []       = return nil
evalExps [x]      = evalExp x
evalExps (x:ESaveContinuation:_) = do
    pad <- ask
    val <- evalExp x
    return . toNative $ mkErr (val, pad)
evalExps (x:xs)   = evalExp x >> evalExps xs

evalExp :: NativeLangExpression -> Eval Native
evalExp ESaveContinuation = do
    pad <- ask
    return . toNative $ mkErr (nil, pad)
evalExp (ELit (NSub s)) = do
    pad <- ask -- close over current scope
    return $ NSub s{ s_pad = pad }
evalExp (ELit n) = return n
evalExp (EVar s) = do
    pad <- ask
    case pad `fetch` s of
        Just v  -> return v
        Nothing -> failWith "No such variable" s
evalExp (ECall { c_obj = objExp, c_meth = meth, c_args = argsExp }) = do
    obj  <- evalExp objExp
    args <- fmapM evalExp argsExp
    if meth == mkStr "trace" then traceObject obj args else do
    case anyPrims `fetch` meth of
        Just f  -> return $ f obj args
        Nothing -> case obj of
            NError {}   -> errMethodMissing
            NBit x | meth == mkStr "cond"
                        -> callConditional x args
            NBit x      -> callMethod bitPrims x args
            NInt x      -> callMethod intPrims x args
            NNum x      -> callMethod numPrims x args
            NStr x      -> callMethod strPrims x args
            NSeq x      -> callMethod seqPrims x args
            NMap x      -> callMethod mapPrims x args
            NSub x      -> callSubWith (toString meth) x args
            NObj x      -> callObject x meth args
    where
    errMethodMissing :: Eval a
    errMethodMissing = failWith "No such method" meth
    callMethod :: MapOf (a -> b -> Native) -> a -> b -> Eval Native
    callMethod prims x args = case prims `fetch` meth of
        Nothing -> errMethodMissing -- XXX - autobox!
        Just f  -> return $ f x args

callSub :: NativeSub -> NativeSeq -> Eval Native
callSub sub args = do
    when (size args /= size prms) $ do
        fail $ "Invalid number of args " ++ show (elems args)
            ++ " vs params " ++ show (elems prms)
            ++ " in subroutine:\n"
            ++ (pretty $ s_exps sub)
    local (append lex . append (s_pad sub)) $ do
        evalExps (elems $ s_exps sub)
    where
    prms = s_params sub
    lex = fromAssocs ((mkStr "&?SUB", toNative sub):elems prms `zip` elems args)

callSubWith :: String -> NativeSub -> NativeSeq -> Eval Native
callSubWith ""          sub args = callSub sub args
callSubWith "do_if"     sub args = if fromNative (args ! 0) then callSub sub empty else return nil
callSubWith "do_unless" sub args = if fromNative (args ! 0) then return nil else callSub sub empty
callSubWith "do_for"    sub args = fmap toNative $ fmapM (callSub sub . mkSeq . (:[])) (fromNative (args ! 0) :: NativeSeq)
callSubWith meth _ _ = failWith "No such method" meth

callConditional :: NativeBit -> NativeSeq -> Eval Native
callConditional x args = callSub (fromNative $ args ! fromEnum (not x)) empty

infixl ...
(...) :: IsNative a => NativeObj -> String -> Eval a
obj ... str = fmap fromNative $ getAttr obj (mkStr str)

traceObject :: Native -> NativeSeq -> Eval Native
traceObject obj args = liftIO $ do
    mapM_ doTrace (obj: elems args)
    return obj
    where
    doTrace x = do
        p <- prettyM x
        putStrLn $ "#trace# " ++ unwords (words (unwords (lines p)))

callObject :: NativeObj -> NativeStr -> NativeSeq -> Eval Native
callObject obj meth args = enterLex lex $ do
    meths <- cls ... "%!methods" :: Eval NativeMap
    case meths `fetch` meth of
        Just x  -> callSub (fromNative x) args
        _       -> tryMRO =<< getMRO
    where
    lex = [("$?SELF", obj), ("$?CLASS", cls)]
    cls = o_class obj
    getMRO = do
        mro <- cls ... "@!MRO" :: Eval NativeSeq
        if isEmpty mro
            then cls ... "@!superclasses"
            else return (elems mro)
    tryMRO (c:cs) = do
        meths <- fromNative c ... "%!methods" :: Eval NativeMap
        case meths `fetch` meth of
            Just x  -> callSub (fromNative x) args
            _       -> tryMRO cs
    tryMRO [] = case objPrims `fetch` meth of
        Just f  -> f obj args
        Nothing -> failWith "no such method" meth

mroMerge :: Eq a => [[a]] -> [a]
mroMerge = reverse . doMerge []
    where
    doMerge res seqs
        | seqs'@(_:_) <- filter (not . null) seqs
        , (cand:_) <- [ s | (s:_) <- seqs', all (not . elem s . tail) seqs']
        = doMerge (cand:res) [ if s == cand then rest else full | full@(s:rest) <- seqs' ]
    doMerge res _ = res

__superclasses, __MRO :: NativeLangMethod
__superclasses  = mkStr "superclasses"
__MRO           = mkStr "MRO"

objPrims :: MapOf (NativeObj -> NativeSeq -> Eval Native)
objPrims = mkMap
    [ ("id", \obj _ -> return (toNative $ o_id obj))
    , ("class", \obj _ -> return (toNative $ o_class obj))
    , ("get_attr", \obj args -> obj ... fromNative (args ! 0))
    , ("set_attr", \obj args -> do
        setAttr obj (fromNative (args ! 0)) (args ! 1)
        return (args ! 1)
      )
    , ("has_attr", \obj args -> do
        val <- hasAttr obj (fromNative (args ! 0))
        return (toNative val)
      )      
    , ("set_attr_hash", \obj args -> do
        let [attrVal, keyVal, val] = elems args
            key :: NativeStr = fromNative keyVal
        hash <- obj ... fromNative attrVal :: Eval NativeMap
        setAttr obj (fromNative attrVal) (toNative $ insert hash key val)
        return val
      )
    , ("new_opaque", \cls args -> fmap toNative $ newObject cls (fromNative $ args ! 0))
    , ("mro_merge", \cls _ -> do
        -- NOTE:
        -- we get the MRO of the object itself
        -- not it's class. In fact, this 
        -- this method should only available
        -- for instances of ::Class. How do we 
        -- handle that??   - stevan
        supers <- fmap fromNative $ callObject cls __superclasses empty
            :: Eval [NativeObj]
        mros   <- fmapM (\c -> fmap fromNative $ callObject c __MRO empty) supers
            :: Eval [[NativeObj]]
        let seqs = filter (not . null) $ ([cls]:mros) ++ [supers]
        return . toNative $ mkSeq (mroMerge seqs)
      )
    ]
