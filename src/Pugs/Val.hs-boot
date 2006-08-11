{-# OPTIONS_GHC -fglasgow-exts -fallow-overlapping-instances #-}

module Pugs.Val where

import Pugs.Internals
import GHC.Exts
import Data.Generics.Basics hiding (cast)
import qualified Data.Typeable as Typeable
import qualified Data.ByteString as Buf

data Val
data ValNative = NBuf !NativeBuf
type NativeBuf      = ByteString
type P = Identity
