{-# OPTIONS_GHC -fglasgow-exts #-}

module PIL where
import PIL.Tie
import PIL.Internals

type Id = Int

-- A Container in Perl 6 is a mutable reference to a pair of
-- (id, content); the content may be tied.
data Box a where
    ScalarBox :: TVar (Id, Tie TiedScalar Scalar) -> Box Scalar
    ArrayBox  :: TVar (Id, Tie TiedArray Array) -> Box Array
    HashBox   :: TVar (Id, Tie TiedHash Hash) -> Box Hash

