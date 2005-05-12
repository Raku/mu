{-# OPTIONS_GHC -cpp -fglasgow-exts -fno-warn-orphans -funbox-strict-fields #-}

module Pugs.AST.Pos (
    Pos(..),
) where
import Data.Typeable

-- |Represents the position of a chunk of source code: filename; start
-- line & column; end line & column.
data Pos = MkPos
    { posName           :: !String
    , posBeginLine      :: !Int
    , posBeginColumn    :: !Int
    , posEndLine        :: !Int
    , posEndColumn      :: !Int
    }
    deriving (Eq, Ord, Typeable)

instance Show Pos where
    show (MkPos name bln bcl eln ecl) = "(MkPos " ++ show name ++ " " ++
        (unwords . map show $ [bln, bcl, eln, ecl]) ++ ")"
