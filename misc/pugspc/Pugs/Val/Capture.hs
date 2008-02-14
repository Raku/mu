{-# OPTIONS_GHC -fglasgow-exts -fparr #-}
module Pugs.Val.Capture where

import GHC.PArr
import Data.Typeable
import Pugs.Internals.ID
import Data.Monoid
import qualified Data.Map as Map

-- | a Capture is a frozen version of the arguments to an application.
data Capt a
    = CaptMeth
        { c_invocant :: a
        , c_feeds    :: [:Feed a:]
        }
    | CaptSub
        { c_feeds    :: [:Feed a:]
        }
    deriving (Show, Eq, Ord, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}


-- | non-invocant arguments.
data Feed a = MkFeed
    { f_positionals :: [: a :]
    , f_nameds      :: Map.Map ID [: a :] 
        -- ^ maps to [:a:] and not a since if the Sig stipulates
        --   @x, "x => 1, x => 2" constructs @x = (1, 2).
    }
    deriving (Show, Eq, Ord, Typeable) {-!derive: YAML_Pos, Perl6Class, MooseClass!-}

instance Monoid [: a :] where
    mempty = [: :]
    mappend = (+:+)

instance Monoid (Feed a) where
    mempty = MkFeed mempty mempty
    mappend (MkFeed x1 x2) (MkFeed y1 y2) = MkFeed (mappend x1 y1) (mappend x2 y2)
    mconcat xs = MkFeed (mconcat (map f_positionals xs)) (mconcat (map f_nameds xs))

emptyFeed :: Feed a
emptyFeed = mempty

concatFeeds :: [: Feed a :] -> Feed a
concatFeeds xs = MkFeed (concatMapP f_positionals xs) (foldlP Map.union mempty (mapP f_nameds xs))
