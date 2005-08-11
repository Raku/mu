{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances -fallow-incoherent-instances -fallow-overlapping-instances #-}

module PIL.Monads where
import PIL.Internals

type Eval = IO

class (Monad m1, Monad m2) => MonadRunM m1 m2 where
    runM :: m1 c -> m2 c

instance (Monad a) => MonadRunM a a where
    runM = id

