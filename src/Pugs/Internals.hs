{-# OPTIONS_GHC -fglasgow-exts -fno-warn-orphans -fno-full-laziness -fno-cse -fno-warn-deprecations -fallow-undecidable-instances -fallow-overlapping-instances -funbox-strict-fields #-}

{-|
    Internal utilities and library imports.

>   Though here at journey's end I lie
>   in darkness buried deep,
>   beyond all towers strong and high,
>   beyond all mountains steep,
>   above all shadows rides the Sun
>   and Stars for ever dwell:
>   I will not say the Day is done,
>   nor bid the Stars farewell.
-}

module Pugs.Internals (
    module Pugs.Internals.Cast,
    module Pugs.Internals.ID,
    module Pugs.Internals.String,
    module Pugs.Internals.Monads,
    module Pugs.Internals.Global,

    module Control.Concurrent,
    module Control.Concurrent.STM,
    module Control.Exception,
    module Control.Monad,
    module Control.Monad.Error,
    module Control.Monad.RWS,
    module Control.Monad.Identity,
    module Data.Array,
    module Data.Bits,
    module Data.Char,
    module Data.Complex,
    module Data.ByteString,
    module Data.Dynamic,
    module Data.Generics,
    module Data.Either,
    module Data.IntMap,
    module Data.IORef,
    module Data.List,
    module Data.Map,
    module Data.Maybe,
    module Data.Ratio,
    module Data.Set,
    module Data.Sequence,
    module Data.Tree,
    module Data.Unique,
    module Data.Word,
    module Debug.Trace,
    module GHC.Conc,
    module GHC.Exts,
    module Network,
    module Numeric,
    module Pugs.Compat,
    module RRegex,
    module RRegex.Syntax,
    module System.Cmd,
    module System.Directory,
    module System.Environment,
    module System.Exit,
    module System.IO,
    module System.IO.Error,
    module System.IO.Unsafe,
    module System.Mem,
    module System.Mem.Weak,
    module System.Process,
    module System.Random,
    module System.Time
) where

import Pugs.Internals.Cast
import Pugs.Internals.ID
import Pugs.Internals.String
import Pugs.Internals.Monads
import Pugs.Internals.Global

import Pugs.Compat
import RRegex
import RRegex.Syntax
import Data.Char
import Data.IORef
import Data.Dynamic hiding (cast)
import Data.Generics (Data)
import Data.Array (elems)
import Network
import Numeric (showHex)
import System.Environment (getArgs, withArgs, getProgName)
import System.Random hiding (split)
import System.Exit
import System.Time
import System.Cmd
import System.Process
import System.IO (
    Handle, stdin, stdout, hClose, hGetLine, hGetChar, hGetContents,
    openFile, hSetBinaryMode, hPutStr, hPutStrLn, IOMode(..), stderr, SeekMode(..),
    hSetBuffering, BufferMode(..), hIsTerminalDevice, hFlush, hPrint, isEOF,
    hSeek, hTell, hIsOpen, hIsClosed, hIsReadable, hIsWritable, hIsSeekable,
    )
import System.IO.Unsafe
import System.IO.Error (ioeGetErrorString, isUserError)
import System.Mem
import System.Mem.Weak
import System.Directory (Permissions(..), getPermissions, getTemporaryDirectory, createDirectory, removeDirectory, removeFile, getDirectoryContents, getModificationTime)
import Control.Exception (catchJust, errorCalls, Exception(..))
import Control.Monad (replicateM, forM, forM_)
import Control.Monad.RWS (MonadIO(..), MonadReader(..), MonadState(..), MonadWriter(..), MonadTrans(..), asks, ReaderT(..), WriterT(..), when, join, liftM, filterM, modify, unless, gets, foldM, guard, liftM2, liftM3, fix, mplus, mappend, mzero, mconcat, msum, censor)
import Control.Monad.Identity (Identity(..))
import Control.Monad.Error (MonadError(..), ErrorT(..), Error(..))
import Control.Concurrent
import Control.Concurrent.STM
import Data.Bits hiding (shift)
import Data.Maybe
import Data.Either
import Data.List (
    (\\), find, genericLength, insert, sortBy, intersperse,
    partition, group, sort, genericReplicate, isPrefixOf, isSuffixOf,
    genericTake, genericDrop, unfoldr, nub, nubBy, transpose, delete, foldl'
    )
import Data.Unique
import Data.Ratio
import Data.Word hiding (Word)
import Data.Complex
import Data.ByteString (ByteString)
import Data.Tree
import qualified Data.Typeable as Typeable
import Data.Set (Set)
import Data.Map (Map)
import Data.Sequence (Seq, singleton)
import Data.IntMap (IntMap)
import Debug.Trace
import GHC.Conc (unsafeIOToSTM)
import GHC.Exts (unsafeCoerce#, Word(W#), Word#)
import qualified Data.Sequence as Seq

import qualified UTF8
import qualified Foreign as Foreign

-- Instances.
instance Show Unique where
    show = show . hashUnique
instance (Typeable a, Typeable b) => Show (a -> b) where
    show _ = "(" ++ typA ++ " -> " ++ typB ++ ")"
        where
        typA = show $ typeOf (undefined :: a)
        typB = show $ typeOf (undefined :: b)
instance (Typeable a, Typeable b) => Eq (a -> b) where
    x == y = show x == show y
instance (Typeable a, Typeable b) => Ord (a -> b) where
    compare x y = compare (show x) (show y)
instance Eq Dynamic where
    x == y = show x == show y
instance Ord Dynamic where
    compare x y = compare (show x) (show y)
