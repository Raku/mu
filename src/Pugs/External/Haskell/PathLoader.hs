----------------------------------------------------------------------------
-- |
-- Module      :  PathLoader
-- Copyright   :  (c) Hampus Ram 2004
-- License     :  BSD-style (see LICENSE)
-- 
-- Maintainer  :  d00ram@dtek.chalmers.se
-- Stability   :  experimental
-- Portability :  non-portable (ghc >= 6 only)
--
-- A module that implements dynamic loading. 
-- Has smart handling of dependencies and
-- is thread safe.
--
----------------------------------------------------------------------------
module Pugs.External.Haskell.PathLoader (LoadedModule,
                                 ModuleType (..),
                                 setBasePath,
                                 addDependency,
                                 setDependencies,
                                 delDependency,
                                 delAllDeps,
                                 withDependencies,
                                 loadModule,
                                 unloadModule,
                                 unloadModuleQuiet,
                                 loadFunction,
                                 loadQualifiedFunction,
                                 moduleLoadedAt,
                                 loadedModules,
                                 DL.addDLL) where

import Control.Monad
import Control.Concurrent.MVar
import Data.List
import qualified Data.HashTable as HT
import Data.IORef
import System.IO.Error
import System.IO.Unsafe
import System.Directory
import System.Time

import qualified External.Haskell.DynamicLoader as DL

data LoadedModule = LM FilePath ModuleType

data ModuleType = MT_Module
                | MT_Package
                  deriving (Eq, Ord, Show)

type ModuleWT = (ModuleType, FilePath)

type PathDynamics = Either DL.DynamicModule DL.DynamicPackage

type PathDep = [ModuleWT]

-- PM reference_count type time module
data PathModule = PM { pm_refc   :: !Int,
                        pm_time   :: ClockTime,
                        pm_deps   :: PathDep,
                        pm_module :: PathDynamics }


-- base_path dependency_map modules
type PathEnvData = (Maybe FilePath,
                    HT.HashTable String [ModuleWT],
                    HT.HashTable String PathModule)


{- 

   New PathEnv that uses bot an IORef and a MVar
   to make it possible to have non blocking functions
   that inspect the state.

-}
type PathEnv = (MVar (), IORef PathEnvData)

withPathEnv :: PathEnv -> (PathEnvData -> IO b) -> IO b
withPathEnv (mvar, ioref) f
    = withMVar mvar (\_ -> readIORef ioref >>= f)

withPathEnvNB :: PathEnv -> (PathEnvData -> IO b) -> IO b
withPathEnvNB (_, ioref) f = readIORef ioref >>= f

modifyPathEnv_ :: PathEnv -> (PathEnvData -> IO PathEnvData) -> IO ()
modifyPathEnv_ (mvar, ioref) f
    = withMVar mvar (\_ -> readIORef ioref >>= f >>= writeIORef ioref)

{-# NOINLINE env #-}
env :: PathEnv
env = unsafePerformIO (do modh <- HT.new (==) HT.hashString
                          deph <- HT.new (==) HT.hashString
                          mvar <- newMVar ()
                          ioref <- newIORef (Nothing, deph, modh)
                          return (mvar, ioref))

{-|

Set the base path used in figuring out module names. If not set the default
(i.e. currentDirectory) will be used.

-}
setBasePath :: Maybe FilePath -> IO ()
setBasePath mpath
    =  modifyPathEnv_ env (\(_, deph, modh) -> return (mpath, deph, modh))


{-|

Add a module dependency. Any dependencies must be added /before/ any
calls to loadModule\/loadPackage or symbols will not be resolved with a
crash as result.

-}
addDependency :: FilePath -> (ModuleType, FilePath) -> IO ()
addDependency from to = withPathEnv env (addDependency' from to)

addDependency' :: FilePath -> (ModuleType, FilePath) -> PathEnvData -> IO ()
addDependency' from to (_, deph, _)
    = insertHT_C union deph from [to]

{-|

Set all dependencies. All previous dependencies are removed.

-}

setDependencies :: FilePath -> [(ModuleType, FilePath)] -> IO ()
setDependencies from to = withPathEnv env (setDependencies' from to)

setDependencies' :: FilePath -> [(ModuleType, FilePath)] -> 
                    PathEnvData -> IO ()
setDependencies' from to (_, deph, _)
    = insertHT deph from to

{-|

Delete a module dependency.

-}
delDependency :: FilePath -> (ModuleType, FilePath) -> IO ()
delDependency from to = withPathEnv env (delDependency' from to)

delDependency' :: FilePath -> (ModuleType, FilePath) -> PathEnvData -> IO ()
delDependency' from to (_, deph, _)
    = modifyHT (\\[to]) deph from

{-|

Delete all dependencies for a module. Same behaviour as
@setDependencies path []@.

-}

delAllDeps :: FilePath -> IO ()
delAllDeps from = withPathEnv env (delAllDeps' from)

delAllDeps' :: FilePath -> PathEnvData -> IO ()
delAllDeps' from (_, deph, _)
    = deleteHT deph from

{-|

Do something with the current dependencies of a module. You can't use
(blocking) functions from this module in the function given to
withDependencies. If you do so, a deadlock will occur.

-}
withDependencies :: FilePath -> 
                    (Maybe [(ModuleType, FilePath)] -> IO a) -> IO a
withDependencies from f
    = withPathEnv env (\(_,deph,_) -> lookupHT deph from >>= f)

{-|

Load a module (or package) and modules (or packages) it depends on. It
is possible to load a module many times without any error
occuring. However to unload a module one needs to call @unloadModule@
the same number of times.

Before loading any modules you should add wich dependencies it has
with addDependency (and which dependencies the modules upon which it
depends have).

If the module already has been loaded nothing will be done except
updating the reference count. I.e. if dependencies have been updated
they will be ignored until the module has been completely unloaded and
loaded again.

If any error occurs an exception is thrown.

-}
loadModule :: FilePath -> ModuleType -> IO LoadedModule
loadModule m mt
    = do withPathEnv env (\env -> do loadModuleWithDep (mt, m) env
                                     DL.resolveFunctions
                                     return (LM m mt))

loadModuleWithDep :: (ModuleType, FilePath) -> PathEnvData -> IO ()
loadModuleWithDep nwt@(_, name) env@(_, _, modh)
    = do mpm <- lookupHT modh name
         (pm, depmods) <- midLoadModule mpm nwt env

         insertHT modh name pm

         mapM_ (\modwt -> loadModuleWithDep modwt env) depmods

midLoadModule :: Maybe PathModule -> (ModuleType, FilePath) -> 
                 PathEnvData -> IO (PathModule, PathDep)
midLoadModule (Just pm) _ _ = return $ (pm { pm_refc = pm_refc pm + 1 },
                                        pm_deps pm)
midLoadModule Nothing nwt@(_, name) env@(_, deph, _)
    = do (sd, time) <- lowLoadModule nwt env
         depmods <- lookupDefHT deph [] name
         return (PM 1 time depmods sd, depmods)

lowLoadModule :: ModuleWT -> PathEnvData -> IO (PathDynamics, ClockTime)
lowLoadModule (MT_Package, name) (_, _, _)
    = do lp <- DL.loadPackageFromPath name
         time <- getModificationTime (DL.dp_path lp)
         return (Right lp, time)
lowLoadModule (MT_Module, name) (mpath, _, _)
    = do lm <- DL.loadModuleFromPath name mpath
         time <- getModificationTime (DL.dm_path lm)
         return (Left lm, time)

{-|

Unload a module and all modules it depends on. This unloading only
occurs if the module isn't needed by any other libraries or hasn't
been loaded more than once. An exception is thrown in case of error.

-}
unloadModule :: LoadedModule -> IO ()
unloadModule (LM name _) 
    = withPathEnv env (unloadModuleWithDep name)

{-|

Same as @unloadModule@ just doesn't trow any exceptions on error.

-}
unloadModuleQuiet :: LoadedModule -> IO ()
unloadModuleQuiet (LM name _)
    = withPathEnv env (\env -> catch (unloadModuleWithDep name env)
                                  (\_ -> return ()))

unloadModuleWithDep :: FilePath -> PathEnvData -> IO ()
unloadModuleWithDep name env@(_, _, modh)
    = do mpm <- lookupHT modh name
         pm <- maybe (fail $ "Module " ++ name ++ " not loaded")
                     return mpm
         
         if pm_refc pm > 1
            then do insertHT modh name (pm { pm_refc = pm_refc pm - 1 })
            else do lowUnloadModule (pm_module pm)
                    deleteHT modh name
 
         mapM_ (\(_, m) -> unloadModuleWithDep m env) (pm_deps pm)

lowUnloadModule :: PathDynamics -> IO ()
lowUnloadModule (Left lm)  = DL.unloadModule lm
lowUnloadModule (Right lp) = DL.unloadPackage lp

{-|

Load a function from a module. It cannot load functions from packages
and will throw an exception if one tries to do so. Also throws if an
error occurs.

It seems (but I'm unsure) like any functions loaded will continue to
be valid even after the module it resides in is unloaded. It will also
still be valid if a new version of that module is loaded (it will thus
still call the old function).

-}
loadFunction :: LoadedModule -> String -> IO a
loadFunction (LM m MT_Module) name
    = withPathEnv env (loadFunction' m name)
loadFunction _ _ = fail "You cannot load functions from a package."

loadFunction' :: String -> String -> PathEnvData -> IO a
loadFunction' mname fname (_, _, modh)
    = do mpm <- HT.lookup modh mname
         pm <- maybe (fail $ "Module " ++ mname ++ " isn't loaded") 
                     return mpm

         let Left dm = pm_module pm
         
         DL.loadFunction dm fname

{-|

Load a qualified function from a module or package. It will throw an
exception if an error occurs. Same restriction as for
DynamicLinker.loadQualifiedFunction applies here too.

-}
loadQualifiedFunction :: String -> IO a
loadQualifiedFunction name
    = withPathEnv env (loadQualifiedFunction' name)

loadQualifiedFunction' :: String -> PathEnvData -> IO a
loadQualifiedFunction' qname _
    = DL.loadQualifiedFunction qname


{-|

Give the modification time for a loded module. Will throw an exception
if the module isn't loaded.

-}
moduleLoadedAt :: LoadedModule -> IO ClockTime
moduleLoadedAt (LM m _)
    = withPathEnvNB env (moduleLoadedAt' m)

moduleLoadedAt' :: FilePath -> PathEnvData -> IO ClockTime
moduleLoadedAt' name (_, _, modh)
    = do mpm <- HT.lookup modh name 
         pm <- maybe (fail $ "Module " ++ name ++ " not loaded")
                     return mpm
         return (pm_time pm)

loadedModules :: IO [String]
loadedModules = withPathEnvNB env loadedModules' 

loadedModules' :: PathEnvData -> IO [String]
loadedModules' (_, _, modh) = HT.toList modh >>= (\lst -> return (map fst lst))

-- functions to handle HashTables in a better way

-- it seems like it doesn't replace the old value on insert
insertHT ht key val 
    = do HT.delete ht key
         HT.insert ht key val

insertHT_C func ht key val
    = do mval <- HT.lookup ht key
         case mval of
                   Just val' -> insertHT ht key (func val val')
                   Nothing   -> insertHT ht key val

modifyHT func ht key
    = do mval <- HT.lookup ht key
         case mval of
                   Just val -> insertHT ht key (func val)
                   Nothing  -> return ()

lookupHT ht key = HT.lookup ht key
deleteHT ht key = HT.delete ht key

lookupDefHT ht val key
    = do mval <- HT.lookup ht key
         case mval of
                   Just val -> return val
                   Nothing  -> return val

