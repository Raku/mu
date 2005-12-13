{-# OPTIONS_GHC -fglasgow-exts -fno-warn-orphans -funbox-strict-fields #-}

module PIL.Native.Eval where
import PIL.Native.Prims
import PIL.Native.Types
import PIL.Native.Coerce
import PIL.Native.Objects
import Data.FunctorM
import Control.Monad.State
import Control.Monad.Reader

{-| 

PIL.Native.Eval

This is an evaluator for the core runtime mini-language.

See Also:

  PIL.Native.Parser
  PIL.Native.Prims  
  PIL.Native.Pretty  

-}

type Eval = StateT ObjectSpace (ReaderT Pad SIO)
type Pad = NativeMap

instance MonadSTM Eval where
    liftSTM = lift . lift . liftSTM

evalNativeLang :: MonadSTM m => [NativeLangExpression] -> m (Native, ObjectSpace)
evalNativeLang = runSIO . (`runReaderT` empty) . (`runStateT` empty) . evalMain

evalMain :: [NativeLangExpression] -> Eval Native
evalMain exps = do
    -- bootstrap
    evalExps exps

evalExps :: [NativeLangExpression] -> Eval Native
evalExps []       = return nil
evalExps [x]      = evalExp x
evalExps (x:xs)   = evalExp x >> evalExps xs

evalExp :: NativeLangExpression -> Eval Native
evalExp (ELit n) = return n
evalExp (EVar s) = do
    pad <- ask
    case pad `fetch` s of
        Just v  -> return v
        Nothing -> fail $ "No such variable " ++ toString s
evalExp (ECall { c_obj = objExp, c_meth = meth, c_args = argsExp }) = do
    obj  <- evalExp objExp
    args <- fmapM evalExp argsExp
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
            NSub x | isEmpty meth
                        -> callSub x args
            NSub x      -> callMethod blockPrims x args
            NObj x      -> callObject x meth args
    where
    errMethodMissing :: Eval a
    errMethodMissing = fail ("No such method: " ++ toString meth)
    callMethod :: MapOf (a -> b -> Native) -> a -> b -> Eval Native
    callMethod prims x args = case prims `fetch` meth of
        Nothing -> errMethodMissing -- XXX - autobox!
        Just f  -> return $ f x args

callSub :: NativeSub -> NativeSeq -> Eval Native
callSub block args = do
    when (size args /= size prms) $ do
        fail $ "Invalid number of args " ++ show (elems args)
            ++ " vs params " ++ show (elems prms)
    local (append lex) $ do
        evalExps (elems $ s_exps block)
    where
    prms = s_params block
    lex = fromAssocs $ elems prms `zip` elems args

callConditional :: NativeBit -> NativeSeq -> Eval Native
callConditional x args = callSub (fromNative $ args ! fromEnum (not x)) empty

callObject :: NativeObj -> NativeStr -> NativeSeq -> Eval Native
callObject obj meth args = do
    mro <- getAttr obj (mkStr "@:MRO")
    return mro
    where
    cls = o_class obj
