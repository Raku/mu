{-# OPTIONS_GHC -fglasgow-exts -fallow-overlapping-instances #-}
module PIL.Repr.Internals (prim, _0, _1, _2, _3) where
import PIL.Native.Types
import PIL.Native.Coerce
import Data.Seq

_0, _1, _2, _3 :: Seq a -> a
_0 = (`index` 0)
_1 = (`index` 1)
_2 = (`index` 2)
_3 = (`index` 3)

prim :: IsPrim a b => x -> (a -> b) -> (x, a -> ObjectPrim)
prim str fun = (str, mkPrim fun)

class IsPrim a b where
    mkPrim :: (a -> b) -> a -> ObjectPrim

instance IsPrim (TVar Native) (STM Native) where
    mkPrim f x _ = f x
instance IsPrim (TVar Native) (Native -> STM ()) where
    mkPrim f x a = f x (index a 0) >> return nil
instance IsNative a => IsPrim (TVar Native) (STM a) where
    mkPrim f x _ = fmap toNative (f x)
