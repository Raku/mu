module Paths_Pugs where
import System.FilePath
import System.Directory
import Pugs.Version (versnum)

getDataFileName :: FilePath -> IO FilePath
getDataFileName fn = do
    _cabal <- getAppUserDataDirectory "cabal"
    createDirectory $ _cabal
    createDirectory $ _cabal </> "share"
    createDirectory $ _cabal </> "Pugs-" ++ versnum
    return $ _cabal </> "share" </> "Pugs-" ++ versnum </> fn
