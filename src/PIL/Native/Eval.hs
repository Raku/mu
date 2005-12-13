{-# OPTIONS_GHC -fglasgow-exts #-}

module PIL.Native.Eval where
import PIL.Native.Prims
import PIL.Native.Types
import PIL.Native.Coerce
import Data.FunctorM
import Control.Monad.Reader

{-| 

PIL.Native.Eval

This is an evaluator for the core runtime mini-language.

See Also:

  PIL.Native.Parser
  PIL.Native.Prims  
  PIL.Native.Pretty  

-}

-- unboxed types have fixed set of methods
-- boxed types can dispatch a lot more

type Pad = NativeMap

evalNativeLang :: Monad m => [NativeLangExpression] -> m Native
evalNativeLang = (`runReaderT` empty) . evalExps

evalExps :: MonadReader Pad m => [NativeLangExpression] -> m Native
evalExps []       = return nil
evalExps [x]      = evalExp x
evalExps (_:xs)   = evalExps xs

evalExp :: forall m. MonadReader Pad m => NativeLangExpression -> m Native
evalExp (NL_Lit n) = return n
evalExp (NL_Var s) = do
    pad <- ask
    case pad `fetch` s of
        Just v  -> return v
        Nothing -> fail $ "No such variable " ++ toString s
evalExp (NL_Call { nl_obj = objExp, nl_meth = meth, nl_args = argsExp }) = do
    obj  <- evalExp objExp
    args <- fmapM evalExp argsExp
    case anyPrims `fetch` meth of
        Just f  -> return $ f obj args
        Nothing -> case obj of
            NError {}-> errMethodMissing
            NBit x   | meth == mkStr "cond" -> callConditional x args
            NBit x   -> callMeth bitPrims x args
            NInt x   -> callMeth intPrims x args
            NNum x   -> callMeth numPrims x args
            NStr x   -> callMeth strPrims x args
            NSeq x   -> callMeth seqPrims x args
            NMap x   -> callMeth mapPrims x args
            NBlock x | isEmpty meth -> callBlock x args
            NBlock x -> callMeth blockPrims x args
    where
    errMethodMissing :: m a
    errMethodMissing = fail ("No such method: " ++ toString meth)
    callMeth :: MapOf (a -> b -> Native) -> a -> b -> m Native
    callMeth prims x args = case prims `fetch` meth of
        Nothing -> errMethodMissing
        Just f  -> return $ f x args
    callBlock :: NativeBlock -> NativeSeq -> m Native
    callBlock block args = do
        when (size args /= size prms) $ do
            fail $ "Invalid number of args " ++ show (elems args)
                ++ " vs params " ++ show (elems prms)
        local (append lex) $ do
            evalExps (elems $ nb_body block)
        where
        prms = nb_params block
        lex = fromAssocs $ elems prms `zip` elems args
    callConditional x args = callBlock (fromNative $ args ! fromEnum (not x)) empty

