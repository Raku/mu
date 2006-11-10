{-# OPTIONS_GHC -fglasgow-exts #-}

module MO.Util ( module MO.Util, trace ) where

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (when, fmap, Functor(..))
import Debug.Trace (trace)
import Data.List (nub)
import Data.Typeable
import GHC.Exts (unsafeCoerce#, Word(W#), Word#)

traceShow :: Show a => a -> b -> b
traceShow = trace . show

traceM :: Monad m => String -> m ()
traceM x = trace x (return ())

-- Compare any two typeable things.
(?==?) :: (Eq a, Typeable a, Typeable b) => a -> b -> Bool
(?==?) x y = case cast y of
    Just y' -> x == y'
    _       -> False

-- Order any two typeable things.
(?<=>?) :: (Ord a, Typeable a, Typeable b) => a -> b -> Ordering
(?<=>?) x y = case cast y of
    Just y' -> x `compare` y'
    _       -> show (typeOf x) `compare` show (typeOf y)

{-# INLINE addressOf #-}
addressOf :: a -> Word
addressOf x = W# (unsafeCoerce# x)

data Ord a => Collection a
    = MkCollection
    { cByObject :: Set a
    , cByName   :: Map String a
    }
    deriving (Eq, Ord, Typeable)


instance (Ord a, Show a) => Show (Collection a) where
    show (MkCollection _ n) = "<" ++ show n ++ ">"

cmap :: (Ord a, Ord b) => (a -> b) -> Collection a -> Collection b
cmap f c@MkCollection { cByName = bn } =
    let l = map (\(x,y) -> (x, f y)) (Map.toList bn)
    in newCollection l
    

-- FIXME: This is not really safe since we could add same object with different
-- names. Must check how Set work and what MO's remove wanted.
remove :: (Monad m, Ord a) => String -> a -> Collection a -> m (Collection a)
remove name obj c@MkCollection{ cByObject = bo, cByName = bn } = do
    return $ MkCollection { cByObject = Set.delete obj bo
                          , cByName = Map.delete name bn
                          } 

add :: (Monad m, Ord a) => String -> a -> Collection a -> m (Collection a)
add name obj c@MkCollection{ cByObject = bo, cByName = bn } = do
    when (includes_name c name) $ fail "can't insert: name confict"
    return $ MkCollection { cByObject = Set.insert obj bo
                          , cByName = Map.insert name obj bn
                          }

insert :: (Ord a) => String -> a -> Collection a -> Collection a
insert name obj c@MkCollection{ cByObject = bo, cByName = bn } =
    MkCollection { cByObject = Set.insert obj bo
                 , cByName = Map.insert name obj bn
                 }

emptyCollection :: Ord a => Collection a
emptyCollection = newCollection []

-- FIXME: checks for repetition
newCollection :: Ord a => [(String, a)] -> Collection a
newCollection l = MkCollection { cByObject = os, cByName = ns }
    where os = Set.fromList (map snd l)
          ns = Map.fromList l

newCollection' :: Ord a => (a -> String) -> [a] -> Collection a
newCollection' f l = newCollection pairs
    where pairs = map (\x -> (f x, x)) l

newCollectionMap :: Ord a => Map String a -> Collection a
newCollectionMap ns = MkCollection { cByObject = os, cByName = ns }
    where os = Set.fromList (Map.elems ns)

items :: Ord a => Collection a -> [a]
items c = Set.elems (cByObject c)

items_named :: Ord a => Collection a -> [(String, a)]
items_named = Map.toList . cByName

includes :: Ord a => Collection a -> a -> Bool
includes c obj = Set.member obj (cByObject c)

includes_name :: Ord a => Collection a -> String -> Bool
includes_name c name = Map.member name (cByName c)

includes_any :: Ord a => Collection a -> [a] -> Bool
includes_any c [] = False
includes_any c (x:xs) = (includes c x) || (includes_any c xs)

includes_any_name :: Ord a => Collection a -> [String] -> Bool
includes_any_name c [] = False
includes_any_name c (x:xs) = (includes_name c x) || (includes_any_name c xs)

includes_all :: Ord a => Collection a -> [a] -> Bool
includes_all c [] = False
includes_all c (x:xs) = (includes c x) && (includes_any c xs)

shadow :: Ord a => [Collection a] -> [a]
shadow = Map.elems . shadow'

shadow' :: Ord a => [Collection a] -> Map String a
shadow' = Map.unions . map cByName

shadow_collection :: Ord a => [Collection a] -> Collection a
shadow_collection = newCollectionMap . shadow'

merge :: Ord a => [Collection a] -> [a]
merge = Map.elems . merge'

merge' :: Ord a => [Collection a] -> Map String a
merge' = foldl (Map.unionWithKey (\k _ _ -> error ("merge conflict: " ++ show k))) Map.empty . map cByName

merge_collection :: Ord a => [Collection a] -> Collection a
merge_collection = newCollectionMap . merge'

sym_shadowing :: (Show a, Ord a) => b -> (b -> [b]) -> (b -> Collection a) -> Collection a
sym_shadowing o parents f = shadow_collection [f o, all_parents]
    where all_parents = sym_merged_parents o parents f

sym_merged_parents :: (Show a, Ord a) => b -> (b -> [b]) -> (b -> Collection a) -> Collection a
sym_merged_parents o parents f = merge_collection cs
    where cs = map (\x -> sym_shadowing x parents f) (parents o)

sym_inheritance :: Ord a => b -> (b -> [b]) -> (b -> (Collection a)) -> Collection a
sym_inheritance o parents f = merge_collection (all_parents ++ [f o])
    where all_parents = map (\p -> sym_inheritance p parents f) (parents o)
