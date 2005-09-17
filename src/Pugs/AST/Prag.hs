{-# OPTIONS_GHC -cpp -fglasgow-exts -fno-warn-orphans -funbox-strict-fields #-}

module Pugs.AST.Prag (
    Pragmas(..),
    Pragma(..),
) where
import Data.Typeable

{-|
Represents lexical pragmas associated with a statement: each of these
is node in a linked list, containing the name of the pragma and some
data associated with this node. For now the data is limited to a native
Int, but we'll figure out how to extend this sometime.

Not to be confused with the 'Exp' constructor of the same name, which stores
a sub-tree and its associated 'Prag'.

XXX: this is currently a cons list, but there's no need. Refactoring
this to be simple list seems likely.
-}
data Pragmas = PrNil
    | PrPrags
      { pragma     :: !Pragma
      , pragmas    :: !Pragmas
      }
    deriving (Show, Eq, Ord, Typeable)

data Pragma = MkPrag
    { pragName           :: !String -- ^ Name of pragma
    , pragDat            :: !Int    -- ^ (lexically scoped) pragmatic data
                                    --     This element is subject to change;
                                    --     we don't necessarily want to limit
                                    --     ourselves to 32 bit ints.
    }
    deriving (Show, Eq, Ord, Typeable)

--instance Show MkPrag where
--    show (MkPrag name dat) = "(MkPrag " ++ show name ++ " " ++
--        (unwords . map show $ [dat]) ++ ")"

