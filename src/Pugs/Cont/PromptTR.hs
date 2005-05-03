{-# OPTIONS -fglasgow-exts #-}

-- A module for generating new prompt names using a name supply.
-- P is the type of computations that might generate new prompts
-- Prompt is the abstract type of prompts
-- runP performs the computation giving a pure answer
-- eqPrompt uses an unsafe coercion

-- Transformer

module PromptTR (
  P, Prompt, runP,
  newPrompt, eqPrompt
) where

import GHC.IOBase
import System.IO.Unsafe

import Control.Monad.State

------------------------------------------------------------------------

type P r m a = StateT Int m a
data Prompt r a = Prompt Int 


runP :: Monad m => (forall r. P r m a) -> m a
runP pe = evalStateT pe 0

newPrompt :: Monad m => P r m (Prompt r a)
newPrompt = get >>= (return . Prompt)

eqPrompt :: Prompt r a -> Prompt r b -> Maybe (a -> b, b -> a)
eqPrompt (Prompt p1) (Prompt p2)  
    | p1 == p2 = Just (coerce id, coerce id)
    | otherwise = Nothing

globalRef :: IORef a 
globalRef = unsafePerformIO $ newIORef undefined

coerce :: a -> b
coerce e = 
    unsafePerformIO $ 
      do writeIORef globalRef e
         readIORef globalRef

------------------------------------------------------------------------
