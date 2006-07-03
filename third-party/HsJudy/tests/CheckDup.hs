import qualified Judy.Map as JM
import Judy.CollectionsM as CM

import Data.Map as DM
import System

-- TODO: make it count the number of dups.

main = do
    s <- getArgs
    if head s == "1"
        then main1
        else main2

main1 = do
    h <- new :: IO (JM.Map String Int)
    loop h
    where loop h = do
            x <- getLine
            v <- CM.lookup x h
            if v == Nothing
                then CM.alter x 1 h >> loop h
                else putStrLn $ "dup: " ++ x

main2 = do
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
