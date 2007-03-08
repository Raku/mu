{-# OPTIONS_GHC -Wall -fno-warn-missing-methods -fno-warn-orphans #-}

import Data.Ord
import Data.Set (Set)
import System.Random
import Test.QuickCheck
import qualified Data.List as List
import qualified Data.Set as Set

newtype Sig = MkSig Int deriving (Show, Eq, Ord)

instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
    arbitrary = fmap Set.fromList arbitrary

instance Arbitrary Sig where
    arbitrary = fmap (MkSig . (`mod` 32768)) arbitrary

sigCompare :: Sig -> Sig -> Ordering
sigCompare sx@(MkSig x) sy@(MkSig y) = case compare x y of
    GT  -> case res `mod` 3 of
        0   -> LT
        1   -> GT
        _   -> EQ
    LT -> case sigCompare sy sx of
        LT  -> GT
        GT  -> LT
        _   -> EQ
    EQ -> EQ
    where
    res = fst . next $ mkStdGen (x * 32768 + y)

dumpcmp :: [Sig] -> String
dumpcmp []     = ""
dumpcmp [_]    = ""
dumpcmp (x:xs) = concatMap (\c -> show x ++ " " ++ (show $ x `sigCompare` c) ++ " " ++ show c ++ "\n") xs

prop_sigCompare :: (Sig, Sig) -> Bool
prop_sigCompare (x, y) = case (sigCompare x y, sigCompare y x) of
    (LT, GT)    -> True
    (GT, LT)    -> True
    (EQ, EQ)    -> True
    _           -> False

prop_dispatch :: Set Sig -> Bool
prop_dispatch xs = case dispatch (Set.toAscList xs) sigCompare of
    Just winner -> winsOverRest winner
    Nothing     -> Set.null (Set.filter winsOverRest xs)
    where
    winsOverRest x = Set.null (Set.filter winsOrTiesWithX rest)
        where
        rest                = Set.delete x xs
        winsOrTiesWithX y   = (sigCompare x y) /= GT

dispatch :: (Eq a, Show a, Monad m) => [a] -> (a -> a -> Ordering) -> m a
dispatch candlist cmp = dispatch' candlist
    where
    dispatch' []        = fail "tied"
    dispatch' (x:y:zs)  = dispatch' $ case cmp x y of
        GT -> x:zs
        LT -> y:zs
        _  -> zs
    dispatch' [x]
        | all (losesToX) spoilers   = return x
        | otherwise                 = fail "spoiled"
        where
        spoilers   = takeWhile (/= x) candlist
        losesToX y = case cmp x y of
            GT  -> True
            _   -> False

main :: IO ()
main = do
    putStrLn "Testing prop_sigCompare"
    quickCheck prop_sigCompare
    putStrLn "Testing prop_dispatch"
    quickCheck prop_dispatch

