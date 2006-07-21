import Atom
import qualified JudyAtom as J
import qualified Data.ByteString.Char8 as B

import System

--main = do
--    let atoms = map toAtom ["jose", "joao", "ana", "luisa", "jose"]
--    putStrLn $ show atoms


main = do
    s <- getArgs
    case read (head s) of
        1 -> main1
        2 -> main2

main1 = do
    x <- getLine
    let !a = toAtom x
    --putStrLn $ show a
    main1

main2 = do
    x <- B.getLine
    let !a = J.toAtom x
    --putStrLn $ show a
    main2
