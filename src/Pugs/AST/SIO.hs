{-# OPTIONS_GHC -cpp -fglasgow-exts -fno-warn-orphans -funbox-strict-fields #-}

module Pugs.AST.SIO (
    MonadSTM(..),

    SIO,
    runSTM, runIO, liftIO,

    module Control.Concurrent.STM
) where
import Control.Concurrent.STM
import Control.Monad.RWS

data SIO a = MkSTM !(STM a) | MkIO !(IO a) | MkSIO !a

runSTM :: SIO a -> STM a
runSTM (MkSTM stm)  = stm
runSTM (MkIO _)     = fail "Unsafe IO caught in STM"
runSTM (MkSIO x)    = return x

runIO :: SIO a -> IO a
runIO (MkIO io)     = io
runIO (MkSTM stm)   = atomically stm
runIO (MkSIO x)     = return x

instance Monad SIO where
    return a = MkSIO a
    (MkIO io)   >>= k = MkIO $ do { a <- io; runIO (k a) }
    (MkSTM stm) >>= k = MkSTM $ do { a <- stm; runSTM (k a) }
    (MkSIO x)   >>= k = k x

-- | Typeclass of monadic types that an @STM@ monad can be lifted to.
class (Monad m) => MonadSTM m where
    runSIO :: SIO a -> m a
    runSIO = fail "runSIO not detailed for this monad"
    liftSTM :: STM a -> m a

instance MonadSTM STM where
    liftSTM = id
    runSIO = runSTM

instance MonadSTM IO where
    liftSTM = atomically
    runSIO = runIO

instance MonadSTM SIO where
    liftSTM stm = MkSTM stm
    runSIO = id

instance MonadIO SIO where
    liftIO io = MkIO io
