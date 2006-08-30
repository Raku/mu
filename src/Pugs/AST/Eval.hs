{-# OPTIONS_GHC -cpp -fglasgow-exts -fno-warn-orphans -fallow-overlapping-instances -funbox-strict-fields -fallow-undecidable-instances #-}

module Pugs.AST.Eval where
import Pugs.Internals
import Pugs.Cont hiding (shiftT, resetT)
import System.IO.Error (try)

import Pugs.AST.SIO
import {-# SOURCE #-} Pugs.AST.Internals

{- Eval Monad -}
type Eval = EvalT (ContT Val (ReaderT Env SIO))
newtype EvalT m a = EvalT { runEvalT :: m a }

instance ((:>:) (Eval a)) (SIO a) where cast = liftSIO

runEvalSTM :: Env -> Eval Val -> STM Val
runEvalSTM env = runSTM . (`runReaderT` enterAtomicEnv env) . (`runContT` return) . runEvalT

runEvalIO :: Env -> Eval Val -> IO Val
runEvalIO env = runIO . (`runReaderT` env) . (`runContT` return) . runEvalT

tryIO :: a -> IO a -> Eval a
tryIO err = lift . liftIO . (`catch` (const $ return err))

{-|
'shiftT' is like @callCC@, except that when you activate the continuation
provided by 'shiftT', it will run to the end of the nearest enclosing 'resetT',
then jump back to just after the point at which you activated the continuation.

Note that because control eventually returns to the point after the 
subcontinuation is activated, you can activate it multiple times in the 
same block. This is unlike @callCC@'s continuations, which discard the current
execution path when activated.

See 'resetT' for an example of how these delimited subcontinuations actually
work.
-}
shiftT :: ((a -> Eval Val) -> Eval Val)
       -- ^ Typically a lambda function of the form @\\esc -> do ...@, where
       --     @esc@ is the current (sub)continuation
       -> Eval a
shiftT e = EvalT . ContT $ \k ->
    runContT (runEvalT . e $ lift . lift . k) return

{-|
Create an scope that 'shiftT'\'s subcontinuations are guaranteed to eventually
exit out the end of.

Consider this example:

> resetT $ do
>     alfa
>     bravo
>     x <- shiftT $ \esc -> do
>        charlie
>        esc 1
>        delta
>        esc 2
>        return 0
>     zulu x

This will:

  1) Perform @alfa@
  
  2) Perform @bravo@
  
  3) Perform @charlie@
  
  4) Bind @x@ to 1, and thus perform @zulu 1@
  
  5) Fall off the end of 'resetT', and jump back to just after @esc 1@
  
  6) Perform @delta@
  
  7) Bind @x@ to 2, and thus perform @zulu 2@
  
  8) Fall off the end of 'resetT', and jump back to just after @esc 2@
  
  6) Escape from the 'resetT', causing it to yield 0

Thus, unlike @callCC@'s continuations, these subcontinuations will eventually
return to the point after they are activated, after falling off the end of the
nearest 'resetT'.
-}
resetT :: Eval Val -- ^ An evaluation, possibly containing a 'shiftT'
       -> Eval Val
resetT e = lift . lift $
    runContT (runEvalT e) return

instance Monad Eval where
    return a = EvalT $ return a
    m >>= k = EvalT $ do
        a <- runEvalT m
        runEvalT (k a)
    fail str = do
        pos <- asks envPos'
        shiftT . const . return $ errStrPos (cast str) pos

instance MonadTrans EvalT where
    lift x = EvalT x

instance Functor Eval where
    fmap f (EvalT a) = EvalT (fmap f a)

instance MonadIO Eval where
    liftIO io = EvalT (liftIO io)

instance MonadError Val Eval where
    throwError err = do
        pos <- asks envPos'
        shiftT . const . return $ errValPos err pos
    catchError _ _ = fail "catchError unimplemented"

{-|
Perform an IO action and raise an exception if it fails.
-}
guardIO :: IO a -> Eval a
guardIO io = do
    rv <- liftIO $ try io
    case rv of
        Left e -> fail (ioeGetErrorString e)
        Right v -> return v

{-|
Like @guardIO@, perform an IO action and raise an exception if it fails.

If t
supress the exception and return an associated value instead.
-}
guardIOexcept :: MonadIO m => [((IOError -> Bool), a)] -> IO a -> m a
guardIOexcept safetyNet io = do
    rv <- liftIO $ try io
    case rv of
        Right v -> return v
        Left  e -> catcher e safetyNet
    where
    catcher e [] = fail (show e)
    catcher e ((f, res):safetyNets)
        | f e       = return res
        | otherwise = catcher e safetyNets

guardSTM :: STM a -> Eval a
guardSTM stm = do
    rv <- liftSTM $ fmap Right stm `catchSTM` (return . Left)
    case rv of
        Left e -> fail (show e)
        Right v -> return v
    
instance MonadSTM Eval where
    liftSIO = EvalT . lift . lift
    liftSTM stm = do
        atom <- asks envAtomic
        if atom
            then EvalT (lift . lift . liftSTM $ stm)
            else EvalT (lift . lift . liftIO . liftSTM $ stm)

instance MonadReader Env Eval where
    ask       = lift ask
    local f m = EvalT $ local f (runEvalT m)

instance MonadCont Eval where
    -- callCC :: ((a -> Eval b) -> Eval a) -> Eval a
    callCC f = EvalT . callCCT $ \c -> runEvalT . f $ \a -> EvalT $ c a

{-
instance MonadEval Eval

class (MonadReader Env m, MonadCont m, MonadIO m, MonadSTM m) => MonadEval m
--     askGlobal :: m Pad
-}

retError :: (Show a) => String -> a -> Eval b
retError str a = fail $ str ++ ": " ++ show a

