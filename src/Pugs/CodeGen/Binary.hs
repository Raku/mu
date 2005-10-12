{-# OPTIONS_GHC -fglasgow-exts #-}
{-# OPTIONS_GHC -#include "../../UnicodeC.h" #-}

module Pugs.CodeGen.Binary (genBinary) where
import Pugs.Internals
import Pugs.AST
import Pugs.Compile
import Pugs.PIL1
import DrIFT.Binary
import System.IO
import System.Directory

genBinary :: Eval Val
genBinary = do
    penv <- compile () :: Eval PIL_Environment
    liftIO $ do
        tmp         <- getTemporaryDirectory
        (file, fh)  <- openBinaryTempFile tmp "pugs.bin"
        bh          <- openBinIO fh
        put_ bh penv
        hClose fh
        return $ VStr (unlines [file])
