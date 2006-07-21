import qualified Judy.Map as M
import qualified Judy.MapSL as S
import Judy.CollectionsM as C

import Data.Map as DM
import System
import qualified Data.ByteString as B


-- TODO: make it count the number of dups.

main = do
    s <- getArgs
    case read (head s) of
        1 -> main1
        2 -> (new :: IO (M.Map String Int)) >>= mainj getLine
        3 -> (new :: IO (S.MapSL String Int)) >>= mainj getLine
        4 -> (new :: IO (M.Map B.ByteString Int)) >>= mainj B.getLine
        5 -> (new :: IO (S.MapSL B.ByteString Int)) >>= mainj B.getLine

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

mainj getline h = do
    loop h
    where loop h = do
            x <- getline
            v <- C.lookup x h
            if v == Nothing
                then C.insert x 1 h >> loop h
                else putStrLn $ "dup: " ++ (show x)
