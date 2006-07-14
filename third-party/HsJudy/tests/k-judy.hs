-- k-judy.hs
--
-- The Great Computer Language Shootout
-- http://shootout.alioth.debian.org/
--
-- Contributed by Einar Karttunen
-- This is a purely functional solution to the problem.
-- An alternative which keeps a mutable table of occurences would be faster.
--
-- Modified by Caio Marcelo for HsJudy testing
--

import Data.Char
import Data.List
import Numeric
import Control.Monad (when)
import qualified Judy.MapSL as J
import qualified Judy.CollectionsM as C
import Data.Maybe (fromJust)

counts _ _ [] = return ()
counts h k dna = do
    let x = take k dna
    J.alter2 plus' x h
    counts h k (tail dna)

counts' _ _ _ [] = return ()
counts' a h k dna = do
    if (head dna) == a
        then do
            let x = take k dna
            J.alter2 plus' x h
            counts h k (tail dna)
        else counts h k (tail dna)


plus' :: Maybe Int -> Maybe Int
plus' Nothing = Just 1
plus' (Just n) = Just (n+1)

writeFrequencies h k dna = do
    counts h k dna
    l <- C.toList h
    let l' = filter (\(a,b) -> length a == k) l
    let tot = foldr ((+) . fromIntegral . snd) 0.0 l :: Float
    let l'' = sortBy (\(_,x) (_,y) -> y `compare` x) $ map (\x@(a, b) -> (a, (fromIntegral b) * 100.0 / tot)) l'
    mapM_ (\(k,f) -> putStr (k++" "++showFFloat (Just 3) f "\n")) l'' >> putStrLn ""


-- FIXME: This step could be better
writeCount sq dna = do
    h <- C.new :: IO (J.MapSL String Int)
    counts h (length sq) dna
    x <- C.lookup sq h
    putStrLn $ (show (fromJust x)) ++ "\t" ++ sq 

dnaThree = process =<< getContents
    where process ls  = return $ ul $ takeNorm $ tail $ dropComment $ dropOther $ lines ls
          dropOther   = dropWhile (\str -> not (">THREE" `isPrefixOf` str))
          dropComment = dropWhile (\str -> head str == ';')
          takeNorm    = takeWhile (\str -> head str /= '>')
          ul str      = map toUpper $ concat str

main = do three <- dnaThree
          h <- C.new :: IO (J.MapSL String Int)
          writeFrequencies h 1 three
          h <- C.new :: IO (J.MapSL String Int)
          writeFrequencies h 2 three
          mapM_ (\k -> writeCount k three) ["GGT", "GGTA", "GGTATT", "GGTATTTTAATT", "GGTATTTTAATTTATAGT"]

