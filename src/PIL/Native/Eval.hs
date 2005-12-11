{-# OPTIONS_GHC -fglasgow-exts #-}

module PIL.Native.Eval where
import PIL.Native.Prims
import PIL.Native.Types
import PIL.Native.Coerce
import Data.FunctorM
import Pugs.AST.SIO

-- unboxed types have fixed set of methods
-- boxed types can dispatch a lot more

type Pad = NativeMap

evalNativeLang :: MonadSTM m => [NativeLangExpression] -> m Native
evalNativeLang []       = return nil
evalNativeLang [x]      = evalExp x
evalNativeLang (_:xs)   = evalNativeLang xs

evalExp :: MonadSTM m => NativeLangExpression -> m Native
evalExp (NL_Lit n) = return n
evalExp (NL_Var s) = fail $ "No such variable " ++ toString s
evalExp (NL_Call { nl_obj = objExp, nl_meth = meth, nl_args = argsExp }) = do
    obj  <- evalExp objExp
    args <- fmapM evalExp argsExp
    case anyPrims `fetch` meth of
        Just f  -> return $ f obj args
        Nothing -> case obj of
            NError {}-> errMethodMissing
            NBit x   -> callMeth bitPrims x args
            NInt x   -> callMeth intPrims x args
            NNum x   -> callMeth numPrims x args
            NStr x   -> callMeth strPrims x args
            NSeq x   -> callMeth seqPrims x args
            NMap x   -> callMeth mapPrims x args
            NBlock x -> callMeth blockPrims x args
    where
    callMeth :: Monad m => MapOf (a -> b -> Native) -> a -> b -> m Native
    callMeth prims x args = case prims `fetch` meth of
        Nothing -> errMethodMissing
        Just f  -> return $ f x args
    errMethodMissing :: Monad m => m a
    errMethodMissing = fail ("No such method: " ++ toString meth)
