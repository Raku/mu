module Paths_Pugs where
import System.FilePath
import System.Directory
import Pugs.Version (versnum)
import Pugs.Config
import qualified Data.Map as Map

getDataFileName :: FilePath -> IO FilePath
getDataFileName fn = do
    dir <- Map.lookup "sourcedir" config
    rv <- doesFileExist $ dir </> fn
    if rv then return (dir </> fn) else do
        _cabal <- getAppUserDataDirectory "cabal"
        createDirectoryIfMissing True $ _cabal </> "Pugs-" ++ versnum
        return $ _cabal </> "share" </> "Pugs-" ++ versnum </> fn
