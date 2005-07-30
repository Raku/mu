{-# OPTIONS_GHC -fglasgow-exts #-}

module PIL where
import PIL.Tie
import PIL.Internals

-- "Container" comes in two flavours: Untieable and Tieable.
-- Once chosen, there is no way in runtime to revert this decision.
data Container a
    = Con (Box a)
    | TCon (TBox a)

-- "Box" is a typed, mutable reference, comprised of an Id and a
-- storage of that type, which can only be Scalar, Array or Hash.
-- Again, there is no way to cast a Box into another type at runtime.

type BoxId = Int
type Box a = TVar (BoxId, a)

-- "TBox" is like Box, but with an additional field that may
-- contain a dispatch table that intercepts various operations.
type TBox a = TVar (BoxId, a, Tieable a)

data Tieable a where
    Untied     :: Tieable a
    TieScalar  :: TiedScalar -> Tieable Scalar
    TieArray   :: TiedArray  -> Tieable Array
    TieHash    :: TiedHash   -> Tieable Hash
