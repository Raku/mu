import qualified Judy.Map as JM
import qualified Judy.MapSL as JMSL
import Judy.CollectionsM as CM

import Data.Map as DM
import System
import qualified Data.ByteString as B


-- TODO: make it count the number of dups.

main = do
    s <- getArgs
    case read (head s) of
        1 -> main1
        2 -> (new :: IO (JM.Map String Int)) >>= mainj getLine
        3 -> (new :: IO (JMSL.MapSL String Int)) >>= mainj getLine
        4 -> (new :: IO (JM.Map B.ByteString Int)) >>= mainj B.getLine
        5 -> (new :: IO (JMSL.MapSL B.ByteString Int)) >>= mainj B.getLine

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
            v <- CM.lookup x h
            if v == Nothing
                then CM.alter x 1 h >> loop h
                else putStrLn $ "dup: " ++ (show x)
