{-# OPTIONS -cpp -fglasgow-exts #-}
----------------------------------------------------------------------------
-- |
-- Module      :  DynamicLoader
-- Copyright   :  (c) Hampus Ram 2003
-- License     :  BSD-style (see LICENSE)
-- 
-- Maintainer  :  d00ram@dtek.chalmers.se
-- Stability   :  experimental
-- Portability :  non-portable (ghc >= 6 only)
--
-- A module that implements dynamic loading. You can load
-- and use GHC object files and packages dynamically at runtime.
--
----------------------------------------------------------------------------
module External.Haskell.DynamicLoader (
                      DynamicModule,
                      dm_path,
                      DynamicPackage,
                      dp_path,

                      addDLL,

                      loadModule,
                      loadModuleFromPath,
                      loadPackage,
                      loadPackageFromPath,
                      unloadModule,
                      unloadPackage,
                      loadFunction,
                      loadQualifiedFunction,
                      resolveFunctions) where

import Char (ord)
import List
import Monad

import GHC.Exts  hiding (split)
import Foreign.Ptr      (Ptr, nullPtr)
import Foreign.C.String (CString, withCString)
import System.Directory (getCurrentDirectory, doesFileExist)

{-

Foreign imports, hooks into the GHC RTS.

-}

foreign import ccall unsafe "loadObj" 
     c_loadObj :: CString -> IO Int

foreign import ccall unsafe "unloadObj" 
     c_unloadObj :: CString -> IO Int

foreign import ccall unsafe "resolveObjs" 
     c_resolveObjs :: IO Int

foreign import ccall unsafe "lookupSymbol" 
     c_lookupSymbol :: CString -> IO (Ptr a)

foreign import ccall unsafe "addDLL" 
     c_addDLL :: CString -> IO CString

-- split up qualified name so one could easily transform it
-- into A.B.C or A/B/C  depending on contex.
data DynamicModule = RTM { dm_qname :: [String],
                           dm_path  :: FilePath }

data DynamicPackage = RTP { dp_path  :: FilePath,
                            dp_cbits :: Maybe DynamicPackage }

{-|

Dynamically load a shared library (DLL or so). A shared library can't
be unloaded using this interface, if you need it use
System.Posix.DynamicLinker instead.

-}

addDLL :: String -> IO ()
addDLL str = withCString str 
               (\s -> do err <- c_addDLL s
                         unless (err == nullPtr)
                                (fail $ "Unable to load library: " ++ str))

{-|

Load a module given its name (for instance @Data.FiniteMap@), maybe a
path to the base directory and maybe a file extension. If no such path
is given the current working directory is used and if no file suffix
is given \"o\" is used.

If we have our module hierarchy in @\/usr\/lib\/modules@ and we want to
load the module @Foo.Bar@ located in @\/usr\/lib\/modules\/Foo\/Bar.o@ we
could isse the command: 

@loadModule \"Foo.Bar\" (Just \"\/usr\/lib\/modules\") Nothing@

If our current directory was @\/tmp@ and we wanted to load the module
@Foo@ located in the file @\/tmp\/Foo.obj@ we would write:

@loadModule \"Foo\" Nothing (Just \"obj\")@

If it cannot load the object it will throw an exception.

-}
loadModule :: String -> Maybe FilePath -> Maybe String -> IO DynamicModule
loadModule name mpath msuff
    = do base <- maybe getCurrentDirectory return mpath

         let qname = split '.' name
             suff  = maybe "o" id msuff
             path  = base ++ '/' : concat (intersperse "/" qname) ++ 
                     '.' : suff
         ret <- withCString path c_loadObj
         if ret /= 0
            then return (RTM qname path)
            else fail $ "Unable to load module: " ++ path

{-|

Load a module given its full path and maybe a base directory to use in
figuring out the modules hierarchical name. If no base directory is
given, it is set to the current directory.

For instance if one wants to load module @Foo.Bar@ located in 
@\/usr\/modules\/Foo\/Bar.o@ one would issue the command:

@loadModuleWithPath \"\/usr\/modules\/Foo\/Bar.o\" (Just
\"\/usr\/modules\")@

If it cannot load the object it will throw an exception.

-}
loadModuleFromPath :: FilePath -> Maybe FilePath -> IO DynamicModule
loadModuleFromPath path mbase
    = do base <- maybe getCurrentDirectory return mbase

         qual <- dropIsEq base path

         -- not very smart but simple...
         let name = reverse $ drop 1 $ dropWhile (/='.') $ 
                    reverse $ if head qual == '/' then drop 1 qual else qual

             qname = split '/' name

         ret <- withCString path c_loadObj
         if ret /= 0
            then return (RTM qname path)
            else fail $ "Unable to load module: " ++ path

    where dropIsEq [] ys = return ys
          dropIsEq (x:xs) (y:ys)
                   | x == y    = dropIsEq xs ys
                   | otherwise = fail $ "Unable get qualified name from: " 
                                       ++ path
          dropIsEq _ _ = fail $ "Unable get qualified name from: " ++ path

split :: Char -> String -> [String]
split _ ""   = []
split c s    = let (l,s') = break (c==) s
               in l : case s' of []      -> []
                                 (_:s'') -> split c s''

{-|

Load a GHC package such as \"base\" or \"text\". Takes the package name,
maybe a path to the packages, maybe a package prefix and maybe a
package suffix.

Path defaults to the current directory, package preifix to \"HS\" and
package suffix to \"o\".

This function also loads accompanying cbits-packages. I.e. if you load
the package @base@ located in @\/usr\/modules@ using @HS@ and @o@ as
prefix and suffix, @loadPackage@ will also look for the file 
@\/usr\/modules\/HSbase_cbits.o@ and load it if present.

If it fails to load a package it will throw an exception. You will
need to resolve functions before you use any functions loaded.

-}
loadPackage :: String -> Maybe FilePath -> Maybe String -> Maybe String -> 
               IO DynamicPackage
loadPackage name mpath mpre msuff
    = do base <- case mpath of
                            Just a -> return a
                            _      -> getCurrentDirectory

         let path = packageName name base mpre msuff

         ret <- withCString path c_loadObj
         unless (ret /= 0) (fail $ "Unable to load package: " ++ name)

         let cbits_path = packageName (name ++ "_cbits") base mpre msuff

         -- this will generate an extra unnecessary call checking for
         -- FOO_cbits_cbits, but it looks nicer!
         cbitsExist <- doesFileExist cbits_path
         if cbitsExist 
            then do rtp <- loadPackage (name ++ "_cbits") mpath mpre msuff
                    return (RTP path (Just rtp))
            else return (RTP path Nothing)

    where packageName :: String -> FilePath -> Maybe String -> 
                         Maybe String -> FilePath
          packageName name path mpre msuff 
              = let prefix = maybe "HS" id mpre
                    suffix = maybe "o" id msuff
                in path ++ '/' : prefix ++ name ++ '.' : suffix

{-|

Load a GHC package such as \"base\" or \"text\". Takes the full path to
the package.

This function also loads accompanying cbits-packages. I.e. if you load
the package @/usr/modules/HSbase.o@ it will deduce that @o@ is the
suffix and @loadPackageFromPath@ will then also look for the file
@\/usr\/modules\/HSbase_cbits.o@ and load it if present.

If it fails to load a package it will throw an exception. You will
need to resolve functions before you use any functions loaded.

-}
loadPackageFromPath :: FilePath -> IO DynamicPackage
loadPackageFromPath path
    = do ret <- withCString path c_loadObj
         unless (ret /= 0) (fail $ "Unable to load package: " ++ path)

         let cbits_path = cbitsName path

         -- this will generate an extra unnecessary call checking for
         -- FOO_cbits_cbits, but it looks nicer!
         cbitsExist <- doesFileExist cbits_path
         if cbitsExist 
            then do rtp <- loadPackageFromPath cbits_path
                    return (RTP path (Just rtp))
            else return (RTP path Nothing)

    where cbitsName :: FilePath -> String
          cbitsName name
              = let suffix = reverse $! takeWhile (/='.') rname
                    rname  = reverse name
                in reverse (drop (length suffix + 1) rname) ++ 
                   "_cbits." ++ suffix -- wrong but simple...

{-|

Unload a package (such as @base@) and its cbits-package (if
any). Throws an exception if any unloading fails.

-}
unloadPackage :: DynamicPackage -> IO ()
unloadPackage (RTP { dp_path = path, dp_cbits = cbits })
    = do ret <- withCString path c_unloadObj
         unless (ret /= 0) (fail $ "Unable to unload package: " ++ path)
         maybe (return ()) unloadPackage cbits

{-|

Unload a previously loaded module. If it cannot unload it an exception
will be thrown.

-}
unloadModule :: DynamicModule -> IO ()
unloadModule (RTM { dm_path = path })
    = do ret <- withCString path c_unloadObj
         unless (ret /= 0) (fail $ "Unable to unload module: " ++ path)

{-|

Load a function from a given module. If the function can't be found an
exception will be thrown. You should have called resolveFunctions before
you call this.

Beware that this function isn't type-safe in any way!

-}
loadFunction :: DynamicModule -> String -> IO a
loadFunction dm functionName 
    = do Ptr addr <- lookupSymbol (dm_qname dm) functionName
         case addrToHValue# addr of
                  (# hval #) -> return hval

{-|

Load a function from package (or module) given the fully qualified
name (e.g. @Data.FinitMap.emptyFM@). If the function can't be found an
exception will be thrown. You should have called resolveFunctions
before you call this.

You must take care that you load the function qualified with the name
of the module it's defined in! You can for instance not load
@Data.Bool.not@ because it is only reexported in that module (from
GHC.Base).

Beware that this function isn't type-safe in any way!

-}
loadQualifiedFunction :: String -> IO a
loadQualifiedFunction functionName
    = do let qfunc = split '.' functionName
         Ptr addr <- lookupSymbol (init qfunc) (last qfunc)
         case addrToHValue# addr of
                  (# hval #) -> return hval

{-|

Resolve all loaded functions. Should be called before any functions
are loaded. If it is unable to resolve all functions it will throw an
exception.

-}
resolveFunctions :: IO ()
resolveFunctions 
    = do ret <- c_resolveObjs
         when (ret == 0) (fail "Unable to resolve functions!")

{-|

Find a symbol in a modules symbol-table. Throw an exception if it
isn't found.

-}
lookupSymbol :: [String] -> String -> IO (Ptr a)
lookupSymbol qname functionName
    = do ptr <- withCString symbolName c_lookupSymbol
         if ptr /= nullPtr
            then return ptr
            else fail $ "Could not load symbol: " ++ symbolName
    where 
    moduleName = encode $ concat (intersperse "." qname)
    realFunctionName = encode functionName

    -- On Mac OSX all functions have an extra _, at least that
    -- is what people say. Not tested!
#ifdef __MACOSX__
    symbolName = "_" ++ moduleName ++ "_" ++ realFunctionName ++ "_closure"
#else
    symbolName = moduleName ++ "_" ++ realFunctionName ++ "_closure"
#endif

    encode :: String -> String
    encode str = concatMap encode_ch str

    unencodedChar :: Char -> Bool -- True for chars that don't need encoding
    unencodedChar 'Z' = False
    unencodedChar 'z' = False
    unencodedChar c   = c >= 'a' && c <= 'z'
                        || c >= 'A' && c <= 'Z'
		        || c >= '0' && c <= '9'

    encode_ch c | unencodedChar c = [c]	-- Common case first
    encode_ch 'Z'  = "ZZ"
    encode_ch 'z'  = "zz"
    encode_ch '&'  = "za"
    encode_ch '|'  = "zb"
    encode_ch '^'  = "zc"
    encode_ch '$'  = "zd"
    encode_ch '='  = "ze"
    encode_ch '>'  = "zg"
    encode_ch '#'  = "zh"
    encode_ch '.'  = "zi"
    encode_ch '<'  = "zl"
    encode_ch '-'  = "zm"
    encode_ch '!'  = "zn"
    encode_ch '+'  = "zp"
    encode_ch '\'' = "zq"
    encode_ch '\\' = "zr"
    encode_ch '/'  = "zs"
    encode_ch '*'  = "zt"
    encode_ch '_'  = "zu"
    encode_ch '%'  = "zv"
    encode_ch c    = 'z' : shows (ord c) "U"
