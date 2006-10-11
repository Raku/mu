{-# OPTIONS -fallow-undecidable-instances -fallow-incoherent-instances #-}

module Judy.Refeable (
    Refeable (..)
) where



import Foreign.StablePtr

import Foreign.Ptr

import Judy.Private
import qualified Judy.MiniGC as GC
import GHC.Exts (unsafeCoerce#)

-- FIXME: It results in an illegal instruction if I take the "Dummy a"
-- out of "Refeable a" context. Maybe something arch related, dunno. =P

--class Dummy a
--instance Dummy a

class Refeable a where
    toRef :: a -> IO Value
    toRef = GC.newRef
    fromRef :: Value -> IO a
    fromRef = deRefStablePtr . castPtrToStablePtr . wordPtrToPtr
    needGC :: a -> Bool
    needGC _ = True
    
--instance Dummy a => Refeable a where
instance Refeable a where

instance Refeable Int where
    toRef i = return $ unsafeCoerce# i
    fromRef v = return $ unsafeCoerce# v
    needGC _ = False


