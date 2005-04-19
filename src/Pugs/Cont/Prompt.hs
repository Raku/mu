{-# OPTIONS_GHC -fglasgow-exts #-}

module Pugs.Cont.Prompt (
    P, Prompt, runP,
    newPrompt, eqPrompt
) where

import GHC.Exts (unsafeCoerce#, eqAddr#)

data P r a = P (Int -> (Int, a))
data Prompt r a = Prompt Int

instance Monad (P r) where
    return e = P (\s -> (s,e))
    (P e1) >>= e2 = P (\s1 -> let (s2,v1) = e1 s1
                                  P f2 = e2 v1
                              in f2 s2)

runP :: (forall r. P r a) -> a
runP pe = let P e = pe in snd (e 0)

newPrompt :: P r (Prompt r a)
newPrompt = P (\s -> (s+1, Prompt s))

eqPrompt :: Prompt r a -> Prompt r b -> Maybe (a -> b, b -> a)
eqPrompt (Prompt p1) (Prompt p2)
    | p1 `unsafePtrEq` p2 = Just (unsafeCoerce# id, unsafeCoerce# id)
    | otherwise = Nothing

unsafePtrEq :: a -> a -> Bool
unsafePtrEq a b = (unsafeCoerce# a) `eqAddr#` (unsafeCoerce# b)
