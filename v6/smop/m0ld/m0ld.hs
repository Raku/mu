import Text.ParserCombinators.Parsec hiding (label)
import qualified Data.Map as Map
import System.IO hiding (getContents,putStrLn,print,putStr)
import Prelude hiding (getContents,putStrLn,print,putStr)
import System.IO.UTF8
import Debug.Trace
import System.Console.GetOpt
import System.Environment
import M0ld
import M0ld.AST
import M0ld.Parser

main = do
    args <- getArgs
    let (options,nonoptions,errors) =  getOpt RequireOrder [Option [] ["print-bytecode"] (NoArg "print-bytecode") "print resulting mold bytecode in a human readable form"] args 
    mapM putStr errors
    hFlush stdout
    line <- getContents
    putStr $ dumpToC $ parseM0ld line
