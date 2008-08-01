module Paths_Pugs where
import System.FilePath
import System.Environment.FindBin

getDataFileName :: FilePath -> IO FilePath
getDataFileName = return . (__Bin__ </>)
