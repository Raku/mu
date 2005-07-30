{-# OPTIONS_GHC -fglasgow-exts #-}

module PIL where
import PIL.Tie
import PIL.Internals

type Id = Int

-- A Container in Perl 6 is a mutable reference to a pair of
-- (id, content); the content may be tied.
data Container a where
    ScalarBox :: TVar (Box Scalar TiedScalar) -> Container Scalar
    ArrayBox  :: TVar (Box Array TiedArray) -> Container Array
    HashBox   :: TVar (Box Hash TiedHash) -> Container Hash

data Box a b = MkBox
    { ident :: Id
    , boxed :: a
    , tied  :: b
    }
