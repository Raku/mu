{-# OPTIONS -fglasgow-exts -O #-}

module Internals.Map (
    FiniteMap,
    emptyFM, addToFM, delFromFM, lookupFM,
    listToFM, fmToList, sizeFM, lookupWithDefaultFM
) where

import qualified Data.Map as Map

type FiniteMap = Map.Map

emptyFM :: FiniteMap k v
emptyFM = Map.empty

addToFM :: (Ord k) => FiniteMap k v -> k -> v -> FiniteMap k v
addToFM fm key val = Map.insert key val fm

delFromFM :: (Ord k) => FiniteMap k v -> k -> FiniteMap k v
delFromFM fm key = Map.delete key fm

lookupFM :: (Ord k) => FiniteMap k v -> k -> Maybe v
lookupFM fm key = Map.lookup key fm

listToFM :: Ord k => [(k, v)] -> FiniteMap k v
listToFM list = Map.fromList list

fmToList :: FiniteMap k v -> [(k, v)]
fmToList fm = Map.toList fm

sizeFM :: FiniteMap k v -> Int
sizeFM fm = Map.size fm

lookupWithDefaultFM :: Ord k => FiniteMap k v -> v -> k -> v
lookupWithDefaultFM fm val key = Map.findWithDefault val key fm
