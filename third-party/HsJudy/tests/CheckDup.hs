import qualified Judy.Hash as H
import qualified Judy.StrMap as S
import Judy.CollectionsM as C

import Data.Map as DM
import System
import qualified Data.ByteString.Char8 as B
import qualified Data.HashTable as H


-- TODO: make it count the number of dups.

main = do
    s <- getArgs
    case read (head s) of
        1 -> main1
        2 -> main2
        3 -> (new :: IO (H.Hash String Int)) >>= mainj getLine
        4 -> (new :: IO (S.StrMap String Int)) >>= mainj getLine
        5 -> (new :: IO (H.Hash B.ByteString Int)) >>= mainj B.getLine
        6 -> (new :: IO (S.StrMap B.ByteString Int)) >>= mainj B.getLine
        7 -> (H.new (==) H.hashString :: IO (H.HashTable String Int)) >>= mainh getLine
        8 -> (H.new (==) (H.hashString . B.unpack) :: IO (H.HashTable B.ByteString Int)) >>= mainh B.getLine

main1 = do
    let h = DM.empty :: (Map String Int)
    loop h
    where loop h = do
            x <- getLine
            let v = DM.lookup x h
            if v == Nothing
                then do
                    let h' = DM.insert x 1 h
                    loop h'
                else putStrLn $ "dup: " ++ x

main2 = do
    let h = DM.empty :: (Map B.ByteString Int)
    loop h
    where loop h = do
            x <- B.getLine
            let v = DM.lookup x h
            if v == Nothing
                then do
                    let h' = DM.insert x 1 h
                    loop h'
                else putStrLn $ "dup: " ++ (show x)

mainj getline h = do
    loop h
    where loop h = do
            x <- getline
            v <- C.lookup x h
            if v == Nothing
                then C.insert x 1 h >> loop h
                else putStrLn $ "dup: " ++ (show x)

mainh getline h = do
    loop h
    where loop h = do
            x <- getline
            v <- H.lookup h x
            if v == Nothing
                then H.insert h x 1 >> loop h
                else putStrLn $ "dup: " ++ (show x)
