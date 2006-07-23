module Main where

{- build with:
 - util/drift.pl src/Pugs/AST/CapInternals.hs
 - ghc -package-conf third-party/installed/packages.conf -isrc -O0 --make misc/pX/gaal/CITest.hs src/pcre/pcre.o
 -}
import DrIFT.Perl6Class

import Pugs.AST.CapInternals
import Pugs.AST.CapInternals.Instances

{- feed this in your higher-order pipe and smoke it -}

main :: IO ()
main = do
    go (undefined :: Val)
    go (undefined :: Val)
    go (undefined :: ValUndef)
    go (undefined :: ValPure)
    go (undefined :: ValMut)
    go (undefined :: ValIO)

    go (undefined :: Sig)
    go (undefined :: Routine)
    go (undefined :: Code)
    go (undefined :: Exp)
    go (undefined :: ExpControl)
    go (undefined :: Param)
    go (undefined :: ParamAccess)
    go (undefined :: CodeAssoc)
    go (undefined :: MultiVariant)
    go (undefined :: EntryDeclarator)
    go (undefined :: PadEntry)
    go (undefined :: MutObject)
    go (undefined :: Cap )

    go (undefined :: Stmt)
    go (undefined :: Arglist)
    go (undefined :: Var)

    go (undefined :: Magic)
    go (undefined :: VRule)
    go (undefined :: Native)
    go (undefined :: Sign)
    go (undefined :: NativeInt)
    go (undefined :: NativeNum)
    go (undefined :: NativeComplex)
    go (undefined :: PureList)
    go (undefined :: PureRange)
    go (undefined :: MemBuf)
    go (undefined :: PureJunc)
    go (undefined :: JuncType)
    go (undefined :: PurePair)
    go (undefined :: PureMap)
    go (undefined :: Bogus)
    go (undefined :: IOFile)
    go (undefined :: IOSocket)
    go (undefined :: IOThread Val)
    go (undefined :: IOProcess)
    return ()

go :: Perl6Class a => a -> IO ()
go = putStrLn . showPerl6TypeDef

{-
data Val
data ValUndef
data ValPure
data ValMut
data ValIO

data Sig
data Routine
data Code
data Exp
data ExpControl
data Param
data ParamAccess
data CodeAssoc
data MultiVariant
data EntryDeclarator
data PadEntry
data MutObject
data Cap 

data Stmt
data Arglist
data Var

data Magic
data VRule
data Native
data Sign
data NativeInt
data NativeNum
data NativeComplex
data PureList
data PureRange
data MemBuf
data PureJunc
data JuncType
data PurePair
data PureMap
data Bogus
data IOFile
data IOSocket
data IOThread
data IOProcess
data SubType

-}

