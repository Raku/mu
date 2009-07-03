{-# OPTIONS_GHC -fglasgow-exts -cpp #-}
import Text.ParserCombinators.Parsec hiding (label)
import qualified Data.Map as Map
import System.IO hiding (getContents,putStrLn,print,putStr)
import Prelude hiding (getContents,putStrLn,print,putStr)
import System.IO.UTF8
import Debug.Trace
import System.Console.GetOpt
import System.Environment
import M0ld.M0ld (prettyPrintBytecode)
import M0ld.LOST (compileToLOST)
import M0ld.C (dumpToC)
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
spec = [("print-bytecode","print resulting mold bytecode in a human readable form"),
            ("exec","execute the m0ld"),
            ("lost-funcs","compile down to a LOST frame (the functions for the frame)"),
            ("lost-create","compile down to a LOST frame (the expression to create the frame)")]
main = do
    args <- getArgs
    let (options,nonoptions,errors) = getOpt RequireOrder (map (\(opt,desc) -> Option [] [opt] (NoArg opt) desc) spec) args
    mapM putStr errors
    hFlush stdout
    code <- getContents
    if elem "lost-create" options then let 
            (funcs,lost,_) = compileToLOST ("foo",0) $ parseM0ld code in
            putStrLn lost
        else if elem "lost-funcs" options then let 
            (funcs,lost,_) = compileToLOST ("foo",0) $ parseM0ld code in
                putStrLn $ (concat funcs)
        else if elem "print-bytecode" options
        then putStrLn $ prettyPrintBytecode "" $ parseM0ld code
        else if elem "exec" options then eval code
        else putStrLn $ dumpToC $ parseM0ld code
