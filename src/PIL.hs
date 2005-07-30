{-# OPTIONS_GHC -fglasgow-exts #-}

module PIL where
import PIL.Tie
import PIL.Internals

type Id = Int
type Container a = TVar (Box a)

data Box a where
    Untied     :: Id -> a -> Box a
    TieScalar  :: Id -> a -> TiedScalar -> Box Scalar
    TieArray   :: Id -> a -> TiedArray  -> Box Array
    TieHash    :: Id -> a -> TiedHash   -> Box Hash
