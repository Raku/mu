----------------------------------------------------------------------------
-- |
-- Module      :  NameLoader
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
module External.Haskell.NameLoader
                  (Module, LoadedModule,
                   setEnvironment,
                   addDependency,
                   delDependency,
                   delAllDeps,
                   withDependencies,
                   loadModule,
                   reloadModule,
                   unloadModule,
                   unloadModuleQuiet,
                   loadFunction,
                   moduleLoadedAt,

                   DL.addDLL) where

import Char (isUpper)

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

type Module = String

newtype LoadedModule = LM Module

data ModuleType = MT_Module
                | MT_Package
                  deriving (Eq, Ord)

type ModuleWT = (String, ModuleType)

type SmartDynamics = Either DL.DynamicModule DL.DynamicPackage

type SmartDep = [Module]

-- SM reference_count type time module
data SmartModule = SM { sm_refc   :: !Int,
                        sm_time   :: ClockTime,
                        sm_deps   :: SmartDep,
                        sm_module :: SmartDynamics }

-- module_path modudle_suff 
-- pkg_path pkg_prefix pkg_suffix 
-- dependency_map modules
type SmartEnvData = (Maybe FilePath, Maybe String,
                     Maybe FilePath, Maybe String, Maybe String,
                     HT.HashTable String [Module],
                     HT.HashTable String SmartModule)

{- 

   New SmarEnv that uses bot an IORef and a MVar
   to make it possible to have non blocking functions
   that inspect the state.

   Could perhaps change it to only use IORef (with atomicModifyIORef)
   but lets play safe and have an MVar too.

-}
type SmartEnv = (MVar (), IORef SmartEnvData)

withSmartEnv :: SmartEnv -> (SmartEnvData -> IO b) -> IO b
withSmartEnv (mvar, ioref) f
    = withMVar mvar (\_ -> readIORef ioref >>= f)

withSmartEnvNB :: SmartEnv -> (SmartEnvData -> IO b) -> IO b
withSmartEnvNB (_, ioref) f = readIORef ioref >>= f

modifySmartEnv_ :: SmartEnv -> (SmartEnvData -> IO SmartEnvData) -> IO ()
modifySmartEnv_ (mvar, ioref) f
    = withMVar mvar (\_ -> readIORef ioref >>= f >>= writeIORef ioref)

{-# NOINLINE env #-}
env :: SmartEnv
env = unsafePerformIO (do modh <- HT.new (==) HT.hashString
                          deph <- HT.new (==) HT.hashString
                          mvar <- newMVar ()
                          ioref <- newIORef (Nothing, Nothing, Nothing,
                                             Nothing, Nothing, deph, modh)
                          return (mvar, ioref))

{-|

Set the environment in wich all module loading will reside. If this
function isn't called the defaults will be used.

The parameters are: Path to modules, module suffix, path to packages,
package prefix and package suffix. The paths will default to current
directory and the rest (in order) to /o/, /HS/ and /o/.

-}
setEnvironment :: Maybe FilePath -> Maybe String -> 
                  Maybe FilePath -> Maybe String -> Maybe String -> IO ()
setEnvironment mpath msuff ppath ppre psuff
    =  modifySmartEnv_ env (\(_, _, _, _, _, deph, modh) ->
                            return (mpath, msuff, ppath, ppre, psuff,
                                    deph, modh))


{-|

Add a module dependency. Any dependencies must be added /before/ any
calls to loadModule or symbols will not be resolved with a crash as
result.

-}
addDependency :: Module -> Module -> IO ()
addDependency from to = withSmartEnv env (addDependency' from to)

addDependency' :: Module -> Module -> SmartEnvData -> IO ()
addDependency' from to (_, _, _, _, _, deph, _)
    = insertHT_C union deph from [to]

{-|

Delete a module dependency.

-}
delDependency :: Module -> Module -> IO ()
delDependency from to = withSmartEnv env (delDependency' from to)

delDependency' :: Module -> Module -> SmartEnvData -> IO ()
delDependency' from to (_, _, _, _, _, deph, _)
    = modifyHT (\\[to]) deph from

{-|

Delete all dependencies for a module.

-}

delAllDeps :: Module -> IO ()
delAllDeps from = withSmartEnv env (delAllDeps' from)

delAllDeps' :: Module -> SmartEnvData -> IO ()
delAllDeps' from (_, _, _, _, _, deph, _)
    = deleteHT deph from

{-|

Do something with the current dependencies of a module. You can't use
(blocking) functions from this module in the function given to
withDependencies. If you do so, a deadlock will occur.

-}
withDependencies :: Module -> (Maybe [Module] -> IO a) -> IO a
withDependencies from f
    = withSmartEnv env (\(_,_,_,_,_,deph,_) -> lookupHT deph from >>= f)

{-|

Load a module (or package) and modules it depends on. It is possible
to load a module many times without any error occuring. However to
unload a module one needs to call @unloadModule@ the same number of
times.

Before loading any modules you should add wich dependencies it has
with addDependency (and which dependencies the modules upon which it
depends have).

If the module already has been loaded nothing will be done except
updating the reference count. I.e. if dependencies have been updated
they will be ignored until the module has been completely unloaded and
loaded again.

It treats names begining with uppercase letters (such as @Foo.Bar@) as
modules and other names (such as @base@) as packages.

If any error occurs an exception is thrown.

-}
loadModule :: Module -> IO LoadedModule
loadModule m 
    = do withSmartEnv env (\env -> do loadModuleWithDep m env
                                      DL.resolveFunctions
                                      return (LM m))

loadModuleWithDep :: Module -> SmartEnvData -> IO ()
loadModuleWithDep name
                  env@(_, _, _, _, _, _, modh)
    = do msm <- HT.lookup modh name
         (sm, depmods) <- midLoadModule msm name env

         insertHT modh name sm

         mapM_ (\modwt -> loadModuleWithDep modwt env) depmods

midLoadModule :: Maybe SmartModule -> Module -> SmartEnvData ->
                 IO (SmartModule, SmartDep)
midLoadModule (Just sm) _ _ = return $ (sm { sm_refc = sm_refc sm + 1 },
                                        sm_deps sm)
midLoadModule Nothing name env@(_, _, _, _, _, deph, _)
    = do (sd, time) <- lowLoadModule (nameToMWT name) env
         depmods <- lookupDefHT deph [] name
         return (SM 1 time depmods sd, depmods)

lowLoadModule :: ModuleWT -> SmartEnvData -> IO (SmartDynamics, ClockTime)
lowLoadModule (name, MT_Package) (_, _, ppath, ppre, psuff, _, _)
    = do lp <- DL.loadPackage name ppath ppre psuff
         time <- getModificationTime (DL.dp_path lp)
         return (Right lp, time)
lowLoadModule (name, MT_Module) (mpath, msuff, _, _, _, _, _)
    = do lm <- DL.loadModule name mpath msuff
         time <- getModificationTime (DL.dm_path lm)
         return (Left lm, time)

{-|

Unload a module and all modules it depends on. This unloading only
occurs if the module isn't needed by any other libraries or hasn't
been loaded more than once. An exception is thrown in case of error.

-}
unloadModule :: LoadedModule -> IO ()
unloadModule (LM name) 
    = withSmartEnv env (unloadModuleWithDep name)

{-|

Same as @unloadModule@ just doesn't trow any exceptions on error.

-}
unloadModuleQuiet :: LoadedModule -> IO ()
unloadModuleQuiet (LM name)
    = withSmartEnv env (\env -> catch (unloadModuleWithDep name env)
                                  (\_ -> return ()))

unloadModuleWithDep :: Module -> SmartEnvData -> IO ()
unloadModuleWithDep name env@(_, _, _, _, _, _, modh)
    = do msm <- lookupHT modh name
         sm <- maybe (fail $ "Module " ++ name ++ " not loaded")
                     return msm
         
         if sm_refc sm > 1
            then do insertHT modh name (sm { sm_refc = sm_refc sm - 1 })
            else do lowUnloadModule (sm_module sm)
                    deleteHT modh name
 
         mapM_ (\m -> unloadModuleWithDep m env) (sm_deps sm)

lowUnloadModule :: SmartDynamics -> IO ()
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
loadFunction (LM m) name
    = withSmartEnv env (loadFunction' (nameToMWT m) name)

loadFunction' :: ModuleWT -> String -> SmartEnvData -> IO a
loadFunction' (_, MT_Package) _ _ 
    = fail "Cannot load functions from packages"
loadFunction' (mname, _) fname (_, _, _, _, _, _, modh)
    = do msm <- HT.lookup modh mname
         sm <- maybe (fail $ "Module " ++ mname ++ " isn't loaded") 
                     return msm

         let Left dm = sm_module sm
         
         DL.loadFunction dm fname


{-|

Give the modification time for a loded module. Will throw an exception
if the module isn't loaded.

-}
moduleLoadedAt :: LoadedModule -> IO ClockTime
moduleLoadedAt (LM m)
    = withSmartEnvNB env (moduleLoadedAt' m)

moduleLoadedAt' :: Module -> SmartEnvData -> IO ClockTime
moduleLoadedAt' name (_, _, _, _, _, _, modh)
    = do msm <- HT.lookup modh name 
         sm <- maybe (fail $ "Module " ++ name ++ " not loaded")
                     return msm
         return (sm_time sm)

{-|

Do a total reload of the module. Ie. unload the current version and
then load it again. Will throw an exception if something fails.

If the last argument is set to @True@ modules will only be reloaded if
they have changed on disk.

-}
reloadModule :: LoadedModule -> Bool -> IO ()
reloadModule (LM name) crisp
    = withSmartEnv env (\env -> do reloadModuleWithDep name crisp env 
                                   DL.resolveFunctions)

reloadModuleWithDep :: Module -> Bool -> SmartEnvData -> IO ()
reloadModuleWithDep name crisp env@(_, _, _, _, _, _, modh)
    = do msm <- HT.lookup modh name
         (sm, depmods) <- midReloadModule msm name crisp env

         HT.delete modh name
         HT.insert modh name sm

         mapM_ (\modwt -> reloadModuleWithDep modwt crisp env) depmods

midReloadModule :: Maybe SmartModule -> Module -> Bool -> SmartEnvData ->
                   IO (SmartModule, SmartDep)
midReloadModule (Just sm) name crisp env@(_, _, _, _, _, deph, _)
    = do mt <- getModificationTime (sm_path sm)
         if not crisp || sm_time sm < mt 
            then do lowUnloadModule (sm_module sm)
                    (sd, time) <- lowLoadModule  (nameToMWT name) env
                    depmods <- lookupDefHT deph [] name
                    return (SM (sm_refc sm) time depmods sd, depmods)
            else return (sm, sm_deps sm)
midReloadModule Nothing name _ env@(_, _, _, _, _, deph, _)
    = do (sd, time) <- lowLoadModule (nameToMWT name) env
         depmods <- lookupDefHT deph [] name
         return (SM 1 time depmods sd, depmods)



-- Some helper functions 

sm_path :: SmartModule -> FilePath
sm_path sm = case sm_module sm of
                               Left  dm -> DL.dm_path dm
                               Right dp -> DL.dp_path dp

nameToMWT :: String -> ModuleWT
nameToMWT (c:cs) 
    | isUpper c = (c:cs, MT_Module)
    | otherwise = (c:cs, MT_Package)
nameToMWT _ = error "empty module names not allowed"

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
