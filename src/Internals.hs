{-# OPTIONS -fglasgow-exts #-}

{-
    Internal utilities and libraries imports.

    Though here at journey's end I lie
    in darkness buried deep,
    beyond all towers strong and high,
    beyond all mountains steep,
    above all shadows rides the Sun
    and Stars for ever dwell:
    I will not say the Day is done,
    nor bid the Stars farewell.
-}

module Internals (
    module Cont,
    module Posix,
    module Rule.Pos,
    module Data.Dynamic,
    module Data.Unique,
    module System.Environment,
    module System.Random,
    module System.IO,
    module System.IO.Unsafe,
    module System.Exit,
    module System.Time,
    module System.Directory,
    module System.Cmd,
    module Control.Monad.RWS,
    module Control.Monad.Error,
    module Data.Bits,
    module Data.List,
    module Data.Either,
    module Data.Word,
    module Data.Ratio,
    module Data.Char,
    module Data.Set,
    module Data.Tree,
    module Data.Maybe,
    module Data.Complex,
    module Data.FiniteMap,
    module Data.IORef,
    module Debug.Trace,
    internalError
) where

import Cont
import Posix
import Data.Dynamic
import System.Environment
import System.Random
import System.Exit
import System.Time
import System.Cmd
import System.IO hiding (try)
import System.IO.Unsafe
import System.Directory
import Control.Monad.RWS
import Control.Monad.Error (MonadError(..))
import Data.Bits hiding (shift)
import Data.Maybe
import Data.Either
import Data.List hiding (intersect, union)
import Data.Unique
import Data.Ratio
import Data.Word
import Data.Char
import Data.Set
import Data.Ratio
import Data.Complex
import Data.FiniteMap
import Data.Tree
import Data.IORef
import Debug.Trace
import Rule.Pos

-- Instances.
instance Show Unique where
    show = show . hashUnique
instance Show (a -> b) where
    show _ = "(->)"
instance Eq (a -> b) where
    _ == _ = False
instance Ord (a -> b) where
    compare _ _ = LT
instance Show (IORef (FiniteMap String String)) where
    show _ = "{ n/a }"

internalError :: String -> a
internalError s = error $ "Internal error: " ++ s ++ " please file a bug report."
