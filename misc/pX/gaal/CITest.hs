module Main where

{- build with:
 - util/drift.pl src/Pugs/AST/CapInternals.hs
 - ghc -package-conf third-party/installed/packages.conf -isrc -O0 --make misc/pX/gaal/CITest.hs src/pcre/pcre.o
 -}
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as Str
import DrIFT.Perl6Class

import Pugs.AST.CapInternals
import Pugs.AST.CapInternals.Instances
--import CIMod

type Str = Str.ByteString

{- feed this in your higher-order pipe and smoke it -}

main :: IO ()
main = do
    --gopASTTypes
    --gomASTTypes
    p6oDemo
    return ()

{-
    { p_variable    :: Ident         -- ^ E.g. $m above
    , p_types       :: [Class]       -- ^ Static pieces of inferencer-food
                                     --   E.g. Elk above
    , p_constraints :: [Code]        -- ^ Dynamic pieces of runtime-mood
                                     --   E.g. where {...} above
    , p_unpacking   :: Maybe Sig     -- ^ E.g. BinTree $t (Left $l, Right $r)
    , p_default     :: Maybe Exp     -- ^ E.g. $answer? = 42
    , p_label       :: Ident         -- ^ E.g. :mode
    , p_slots       :: Table         -- ^ Any additional attrib not
                                     --   explicitly mentioned below
    , p_hasAccess   :: ParamAccess   -- ^ is ro, is rw, is copy
    , p_isRef       :: Bool
    , p_isLazy      :: Bool
-}
p6oDemo :: IO ()
p6oDemo = do
    putStrLn $ asPerl6Object $ MkParam (Str.pack "$bullwinkle") [] [] Nothing Nothing (Str.pack "bullwinkle") Map.empty AccRW False False

gopASTTypes :: IO ()
gopASTTypes = do
    gop (undefined :: Val)
    gop (undefined :: Val)
    gop (undefined :: ValUndef)
    gop (undefined :: ValPure)
    gop (undefined :: ValMut)
    gop (undefined :: ValIO)

    gop (undefined :: Sig)
    gop (undefined :: Routine)
    gop (undefined :: Code)
    gop (undefined :: Exp)
    gop (undefined :: ExpControl)
    gop (undefined :: Param)
    gop (undefined :: ParamAccess)
    gop (undefined :: CodeAssoc)
    gop (undefined :: MultiVariant)
    gop (undefined :: EntryDeclarator)
    gop (undefined :: PadEntry)
    gop (undefined :: MutObject)
    gop (undefined :: Cap )

    gop (undefined :: Stmt)
    gop (undefined :: Arglist)
    gop (undefined :: Var)

    gop (undefined :: Magic)
    gop (undefined :: VRule)
    gop (undefined :: Native)
    gop (undefined :: Sign)
    gop (undefined :: NativeInt)
    gop (undefined :: NativeNum)
    gop (undefined :: NativeComplex)
    gop (undefined :: PureList)
    gop (undefined :: PureRange)
    gop (undefined :: MemBuf)
    gop (undefined :: PureJunc)
    gop (undefined :: JuncType)
    gop (undefined :: PurePair)
    gop (undefined :: PureMap)
    gop (undefined :: Bogus)
    gop (undefined :: IOFile)
    gop (undefined :: IOSocket)
    gop (undefined :: IOThread Val)
    gop (undefined :: IOProcess)

gomASTTypes :: IO ()
gomASTTypes = do
    gom (undefined :: Val)
    gom (undefined :: Val)
    gom (undefined :: ValUndef)
    gom (undefined :: ValPure)
    gom (undefined :: ValMut)
    gom (undefined :: ValIO)

    gom (undefined :: Sig)
    gom (undefined :: Routine)
    gom (undefined :: Code)
    gom (undefined :: Exp)
    gom (undefined :: ExpControl)
    gom (undefined :: Param)
    gom (undefined :: ParamAccess)
    gom (undefined :: CodeAssoc)
    gom (undefined :: MultiVariant)
    gom (undefined :: EntryDeclarator)
    gom (undefined :: PadEntry)
    gom (undefined :: MutObject)
    gom (undefined :: Cap )

    gom (undefined :: Stmt)
    gom (undefined :: Arglist)
    gom (undefined :: Var)

    gom (undefined :: Magic)
    gom (undefined :: VRule)
    gom (undefined :: Native)
    gom (undefined :: Sign)
    gom (undefined :: NativeInt)
    gom (undefined :: NativeNum)
    gom (undefined :: NativeComplex)
    gom (undefined :: PureList)
    gom (undefined :: PureRange)
    gom (undefined :: MemBuf)
    gom (undefined :: PureJunc)
    gom (undefined :: JuncType)
    gom (undefined :: PurePair)
    gom (undefined :: PureMap)
    gom (undefined :: Bogus)
    gom (undefined :: IOFile)
    gom (undefined :: IOSocket)
    gom (undefined :: IOThread Val)
    gom (undefined :: IOProcess)
    return ()

gop :: Perl6Class a => a -> IO ()
gop = putStrLn . showPerl6TypeDef ("v6::AST::" ++)

gom :: MooseClass a => a -> IO ()
gom = putStrLn . showMooseTypeDef ("v6::AST::" ++)

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

