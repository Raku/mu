{-# OPTIONS_GHC -fglasgow-exts #-}

module Pugs.Cont.CPS (
    M, K,
    throw, c,
    runM
) where

data K ans a = K (a -> ans)
data M ans a = M (K ans a -> ans)

instance Monad (M ans) where
    return e = M (\ (K k) -> k e)
    (M e1) >>= e2 =
        M (\k -> e1 (K (\ v1 -> let M c = e2 v1 in c k)))

callCC :: (K ans a -> M ans a) -> M ans a
callCC f = M (\k -> let M c = f k in c k)

abort :: ans -> M ans a
abort a = M (const a)

throw :: K ans a -> M ans a -> M ans b
throw k (M e) = M (const $ e k)

c :: (K ans a -> ans) -> M ans a
c f = callcc (\k -> abort (f k))

runM :: M ans ans -> ans
runM (M e) = e (K id)
