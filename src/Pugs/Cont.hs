{-# OPTIONS_GHC -fglasgow-exts -fno-warn-unused-binds #-}

{-
    Continuation with shift/reset operators.

    The Road goes ever on and on
    Down from the door where it began.
    Now far ahead the Road has gone,
    And I must follow, if I can...
-}

module Pugs.Cont (
    callCC, shift, reset, shiftT, resetT,
    module Control.Monad.Cont,
) where

import qualified Control.Monad.Cont as C (callCC, lift)
import Control.Monad.Cont (mapContT, withContT, mapCont, withCont, Cont(..), ContT(..), MonadCont)

-- Cont' m a is the type of a continuation expecting an a within the 
-- continuation monad Cont m
type Cont' m a = forall r. a -> m r

callCC :: forall a m. MonadCont m => (Cont' m a -> m a) -> m a
callCC f = C.callCC f' where
  f' :: (a -> m (EmptyMonad m)) -> m a
  f' g = f g' where
    g' :: a -> m b
    g' = (=<<) runEmptyMonad . g

-- ghc doesn't allow something like m (forall c. m c)
newtype EmptyMonad m = EmptyMonad { runEmptyMonad :: forall c. m c }

-- shift/reset for the Cont monad
shift :: ((a -> Cont s r) -> Cont r r) -> Cont r a
shift e = Cont $ \k -> e (return . k) `runCont` id

reset :: Cont a a -> Cont r a 
reset e = return $ e `runCont` id

-- shiftT/resetT for the ContT monad transformer
shiftT :: Monad m => ((a -> ContT r m s) -> ContT s m s) -> ContT s m a
shiftT e = ContT $ \k -> e (C.lift . k) `runContT` return

resetT :: Monad m => ContT a m a -> ContT r m a
resetT e = C.lift $ e `runContT` return

