#!/usr/bin/env runghc
\begin{code}

module Main where
import Distribution.Simple
import Distribution.PreProcess
import Distribution.PackageDescription

main :: IO ()
main = defaultMain

\end{code}

main' :: IO ()
main' = defaultMainWithHooks $ defaultUserHooks
    { runTests = test
    , preBuild = stage1
    , hookedPreProcessors = [ppDrift] -- New and/or overrides
    }

test :: a -> b -> c -> IO d
test _ _ _ = error "foo!"

ppDrift :: a
ppDrift = error "bar!"

-- Basic plan: Here we detect, by virtue of --bootstrap, whether
-- we are to do a stage1 build or skip over to the main build after all.

stage1 :: Args -> Int -> IO HookedBuildInfo
stage1 = error "baz!"
