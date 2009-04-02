{-# OPTIONS_GHC -fglasgow-exts -cpp #-}
import Text.ParserCombinators.Parsec hiding (label)
import qualified Data.Map as Map
import System.IO hiding (getContents,putStrLn,print,putStr)
import Prelude hiding (getContents,putStrLn,print,putStr)
import System.IO.UTF8
import Debug.Trace
import System.Console.GetOpt
import System.Environment
import M0ld.M0ld
import M0ld.AST
import M0ld.Parser
#ifdef EMBED_SMOP
import M0ld.Eval
#endif

#ifdef EMBED_SMOP
eval code = evalM0ld code 
#else
eval code = putStrLn "--exec use ./Setup configure --user --flags=SMOP"
#endif
main = do
    args <- getArgs
    let (options,nonoptions,errors) =  getOpt RequireOrder [Option [] ["print-bytecode"] (NoArg "print-bytecode") "print resulting mold bytecode in a human readable form",Option [] ["exec"] (NoArg "exec") "execute the m0ld"] args 
    mapM putStr errors
    hFlush stdout
    code <- getContents
    if elem "print-bytecode" options
        then putStrLn $ prettyPrintBytecode "" $ parseM0ld code
        else if elem "exec" options then eval code
        else putStrLn $ dumpToC $ parseM0ld code
