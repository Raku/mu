#!/usr/bin/env runghc
> {-# GHC_OPTIONS -fno-warn-deprecation #-}
> import Distribution.Simple
> import System.Directory
> import System.Environment
> -- import System.Cmd (rawSystem)
> 
> main :: IO ()
> main = do
>     args <- getArgs
>     if args == ["-d"] then putStrLn =<< getAppUserDataDirectory "cabal" else do
>--         writeBuildInfo
>         defaultMainWithHooks autoconfUserHooks
>--     where
>--     writeBuildInfo = rawSystem "perl" ["Configure.PL"]
