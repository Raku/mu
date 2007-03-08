import Test.QuickCheck
import Data.Ord
import System.Random
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.List as List
import Debug.Trace

newtype Sig = MkSig Int deriving (Show, Eq, Ord)

instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
    arbitrary = do
        xs <- arbitrary
        return (Set.fromList xs)

instance Arbitrary Sig where
    arbitrary = do
        int <- arbitrary
        return $ MkSig (int `mod` 32768)

cmp :: Sig -> Sig -> Ordering
cmp sx@(MkSig x) sy@(MkSig y)
    | x == y    = EQ
    | x > y     = case cmp sy sx of
        LT  -> GT
        GT  -> LT
        _   -> EQ
    | otherwise = case res `mod` 3 of
        0 -> LT
        1 -> GT
        _ -> EQ
    where
    res = fst . next $ mkStdGen (x * 32768 + y)

dumpcmp :: [Sig] -> String
dumpcmp []     = ""
dumpcmp (x:[]) = ""
dumpcmp (x:xs) = concatMap (\c -> show x ++ " " ++ (show $ x `cmp` c) ++ " " ++ show c ++ "\n") xs


prop_cmp :: (Sig, Sig) -> Bool
prop_cmp (x, y) = case (cmp x y, cmp y x) of
    (LT, GT)    -> True
    (GT, LT)    -> True
    (EQ, EQ)    -> True
    _           -> False

prop_dispatch :: Set Sig -> Bool
prop_dispatch xs = case dispatch xs cmp of
    Just winner -> winsOverRest winner
    Nothing     -> Set.null (Set.filter winsOverRest xs)
    where
    winsOverRest x = Set.null (Set.filter winsOrTiesWithX rest)
        where
        rest                = Set.delete x xs
        winsOrTiesWithX y   = (cmp x y) /= GT

dispatch :: (Eq a, Show a) => Set a -> (a -> a -> Ordering) -> Maybe a
dispatch cand cmp = dispatch' candlist
    where
    candlist         = Set.toList cand
    verify c         = if all ((GT ==) . cmp c) (takeWhile (/= c) candlist) then Just c else Nothing
    dispatch' []     = Nothing -- trace "all-tied" Nothing
    dispatch' (x:[]) = verify x
    -- trace ("pending verification: " ++ show x ++ ", " ++ show (length candlist)) $ Just x
    dispatch' (x:y:zs) = dispatch' $ case cmp x y of
        GT -> x:zs
        LT -> y:zs
        _  -> zs


main :: IO ()
main = do
    putStrLn "Testing prop_cmp"
    quickCheck prop_cmp
    putStrLn "Testing prop_dispatch"
    quickCheck prop_dispatch

