{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances -fallow-incoherent-instances -fallow-overlapping-instances #-}

module PIL.Monads where
import PIL.Internals
import Control.Monad.Identity

type Parse = IO
type Compile = IO
type Eval = IO

{-
class (Monad m) => MonadRun m where
    run :: m a -> a
instance (MonadRun m1, Monad m2) => MonadRunM m1 m2 where
    runM = return . run
-}

class (Monad m1, Monad m2) => MonadRunM m1 m2 where
    runM :: m1 c -> m2 c

instance (Monad a) => MonadRunM a a where
    runM = id

