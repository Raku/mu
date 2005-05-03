{-# OPTIONS -fglasgow-exts #-}

-- A module defining sequences of control segments and prompts
-- The control segments are some sort of continuations but we don't
--   care about their exact nature here. 
-- We can append two sequences and split a sequence at the first
--   occurrence of a given prompt

module SeqTR (
  Seq (..), 
  splitSeq, appendSeq
) where

import qualified PromptTR as Prompt

------------------------------------------------------------------------

data Seq s r a b = EmptyS (a -> b)
                 | PushP (Prompt.Prompt r a) (Seq s r a b)
                 | forall c. PushSeg (s r a c) (Seq s r c b)
                 | forall c. PushCO (a -> c) (Seq s r c b)

splitSeq :: Prompt.Prompt r b -> Seq s r a ans -> (Seq s r a b, Seq s r b ans)
splitSeq p (EmptyS _) = error ("Prompt was not found on the stack")
splitSeq p (PushP p' sk) = 
  case Prompt.eqPrompt p' p of 
    Nothing -> let (subk,sk') = splitSeq p sk
               in (PushP p' subk, sk')
    Just (a2b,b2a) -> (EmptyS a2b, PushCO b2a sk)
splitSeq p (PushSeg seg sk) = 
    let (subk,sk') = splitSeq p sk
    in (PushSeg seg subk, sk')
splitSeq p (PushCO f sk) = 
    let (subk,sk') = splitSeq p sk
    in (PushCO f subk, sk')

appendSeq :: Seq s r a b -> Seq s r b ans -> Seq s r a ans
appendSeq (EmptyS f) sk = PushCO f sk
appendSeq (PushP p subk) sk = PushP p (appendSeq subk sk)
appendSeq (PushSeg seg subk) sk = PushSeg seg (appendSeq subk sk)
appendSeq (PushCO f subk) sk = PushCO f (appendSeq subk sk)

----------------------------------------------------------------------

