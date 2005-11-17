{-# OPTIONS_GHC -fglasgow-exts #-}

module Pugs.Embed.Pugs where
import System.Cmd
import System.Directory
import System.IO

evalPugs :: String -> IO ()
evalPugs str = do
    tmp         <- getTemporaryDirectory
    (file, fh)  <- openTempFile tmp "pugs.hs"
    hPutStr fh str
    hClose fh
    rv          <- findExecutable "runghc"
    case rv of
        Nothing -> removeFile file >> fail "Cannot find runghc in PATH"
        Just p  -> rawSystem p [file] >> removeFile file
