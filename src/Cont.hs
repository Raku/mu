{-# OPTIONS -fglasgow-exts #-}

module Cont (
    callCC, shift, reset, shiftT, resetT,
    module Control.Monad.Cont,
) where

import qualified Control.Monad.Cont as C (callCC)
import Control.Monad.Cont (mapContT, withContT, mapCont, withCont, runCont, Cont(..), ContT(..), runContT, MonadCont)

callCC :: MonadCont m => ((a -> (forall b. m b)) -> m a) -> m a
callCC (f :: ((a -> (forall b. m b)) -> m a) ) = C.callCC f' where
  f' :: (a -> m (EmptyMonad m)) -> m a
  f' g = f g' where
    g' :: a -> m b
    g' = (=<<) runEmptyMonad . g

-- ghc doesn't allow something like m (forall c. m c)
newtype EmptyMonad m = EmptyMonad { runEmptyMonad :: forall c. m c }

-- shift/reset for the Cont monad
shift :: ((a -> Cont r s) -> Cont s s) -> Cont s a
shift e = Cont $ \k -> runCont (e $ \v -> Cont $ \c -> c (k v)) id

reset :: Cont a a -> Cont r a 
reset e = Cont $ \k -> k (runCont e id)

-- shiftT/resetT for the ContT monad transformer
shiftT :: Monad m => ((a -> ContT r m s) -> ContT s m s) -> ContT s m a
shiftT e = ContT $ \k -> runContT (e $ \v -> ContT $ \c -> c =<< k v) return

resetT :: Monad m => ContT a m a -> ContT r m a
resetT e = ContT $ \k -> k =<< runContT e return
