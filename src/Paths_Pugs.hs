module Paths_Pugs where
import System.FilePath
import System.Directory
import Pugs.Version (versnum)
import Pugs.Config
import qualified Data.Map as Map

getDataFileName :: FilePath -> IO FilePath
getDataFileName fn = do
    dir <- Map.lookup "sourcedir" config
    print (dir </> fn)
    rvf <- doesFileExist $ dir </> fn
    rvd <- doesDirectoryExist $ dir </> fn
    if rvf || rvd then return (dir </> fn) else do
        _cabal <- getAppUserDataDirectory "cabal"
        createDirectoryIfMissing True $ _cabal </> "share" </> "Pugs-" ++ versnum
        return $ _cabal </> "share" </> "Pugs-" ++ versnum </> fn
