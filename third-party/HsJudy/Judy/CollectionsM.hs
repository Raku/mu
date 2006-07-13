{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}

module Judy.CollectionsM where 

-- import Prelude (Bool(..), Int, Maybe(..),
--                 (==), (.), (+), ($), (-), (&&), (||),
--                 Eq, Ord, 
--                 error, const, not, fst, snd, maybe, head, otherwise, curry, uncurry, flip,
--                 min, max, Show)

-- import Prelude hiding (sum,concat,lookup,map,filter,foldr,foldr1,foldl,null,reverse,(++),minimum,maximum,all,elem,concatMap,drop,head,tail,init)

{-
class Monad m => CollectionM c i o m | c -> i o m where
    -- From Foldable
    null :: c -> m Bool
    size :: c -> m Int
    
    empty :: m c
    isSingleton :: c -> m Bool
    -- FIXME: create a new structure? or delete inplace? or have both options?
    filter :: (o -> Bool) -> c -> m c
    insert :: i -> c -> m ()
    singleton :: i -> m c

    -- FIXME: Foldable here
    insertMany :: [i] -> c -> m ()
    isSingleton :: c -> m Bool
-}

class MapF c k a | c -> k a where
    memberF :: k -> c -> Bool
    lookupF :: k -> c -> Maybe a
    fromListF :: [(k,a)] -> c
    toListF :: c -> [(k, a)]

class Monad m => MapM c k a m | c -> k a m where
    new :: m c
    delete :: k -> c -> m ()
    member :: k -> c -> m Bool
    lookup :: k -> c -> m (Maybe a) -- FIXME: change Maybe to m'?
    --alter :: (Maybe a -> Maybe a) -> k -> c -> m ()
    alter :: k -> a -> c -> m ()


    -- Generalize more... (fromFoldable, fromListWith, and both)
    --fromFoldableWith :: Foldable l (k,a) => (a -> a -> a) -> l -> m c
    fromList :: [(k,a)] -> m c
    toList :: c -> m [(k,a)]

    --union :: c -> c -> m c
    --intersection :: c -> c -> m c
    --difference :: c -> c -> c
    --isSubset :: c -> c -> m Bool

    --insertWith :: (a -> a -> a) -> k -> a -> c -> m ()
    
    -- FIXME: create a new structure? or delete inplace? or have both?
    --mapWithKey :: (k -> a -> a) -> c -> m c
    --unionWith :: (a -> a -> a) -> c -> c -> m c
    --intersectionWith :: (a -> a -> a) -> c -> c -> m c
    --differenceWith :: (a -> a -> Maybe a) -> c -> c -> m c
    --isSubmapBy :: (a -> a -> Bool) -> c -> c -> m Bool




