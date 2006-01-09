{-# OPTIONS_GHC -fglasgow-exts #-}
module PIL.Repr (Repr, createRepr, callRepr) where
import PIL.Repr.P6Array as P6Array
import PIL.Repr.P6Hash as P6Hash
import PIL.Repr.P6Scalar as P6Scalar
import PIL.Repr.P6Opaque as P6Opaque
import PIL.Native.Coerce
import PIL.Native.Types
import Data.Typeable

data Repr
    = P6Hash !P6Hash
    | P6Array !P6Array
    | P6Scalar !P6Scalar
    | P6Opaque !P6Opaque
    deriving (Typeable)
    
reprTypes :: MapOf (Native -> STM Repr)
reprTypes = mkMap
    [ ("p6hash",   (fmap P6Hash . P6Hash.create))
    , ("p6array",  (fmap P6Array . P6Array.create))
    , ("p6scalar", (fmap P6Scalar . P6Scalar.create))
    , ("p6opaque", (fmap P6Opaque . P6Opaque.create))
    ]

createRepr :: NativeStr -> Native -> STM Repr
createRepr = (reprTypes !)

callRepr :: Repr -> NativeStr -> ObjectPrim
callRepr (P6Hash x) = (P6Hash.prims !)
callRepr (P6Array x) = (P6Array.prims !)
callRepr (P6Scalar x) = (P6Scalar.prims !)
callRepr (P6Opaque x) = (P6Opaque.prims !)

