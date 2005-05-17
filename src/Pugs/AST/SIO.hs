{-# OPTIONS_GHC -cpp -fglasgow-exts -fno-warn-orphans -funbox-strict-fields #-}

module Pugs.AST.SIO (
    MonadSTM,

    SIO,
    runSTM, runIO,
    liftSTM, liftIO,
) where
import Pugs.Internals

data SIO a = MkSTM !(STM a) | MkIO !(IO a) | MkSIO !a
    deriving (Typeable)

runSTM :: SIO a -> STM a
runSTM (MkSTM stm)  = stm
runSTM (MkIO _ )    = fail "Unsafe IO caught in STM"
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
    liftSTM :: STM a -> m a

instance MonadSTM STM where
    liftSTM = id

instance MonadSTM IO where
    liftSTM = atomically

instance MonadIO SIO where
    liftIO io = MkIO io

instance MonadSTM SIO where
    liftSTM stm = MkSTM stm
