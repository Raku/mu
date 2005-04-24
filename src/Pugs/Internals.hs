{-# OPTIONS_GHC -fglasgow-exts -fno-warn-orphans #-}

{-
    Internal utilities and library imports.

    Though here at journey's end I lie
    in darkness buried deep,
    beyond all towers strong and high,
    beyond all mountains steep,
    above all shadows rides the Sun
    and Stars for ever dwell:
    I will not say the Day is done,
    nor bid the Stars farewell.
-}

module Pugs.Internals (
    module UTF8,
    module Unicode,
    module Pugs.Cont,
    module Pugs.Embed,
    module Pugs.Compat,
    module RRegex,
    module RRegex.Syntax,
    module Pugs.Rule.Pos,
    module Data.Dynamic,
    module Data.Unique,
    module Control.Exception,
    module System.Environment,
    module System.Random,
    module System.IO,
    module System.IO.Unsafe,
    module System.IO.Error,
    module System.Exit,
    module System.Time,
    module System.Directory,
    module System.Cmd,
    module Control.Monad.RWS,
    module Control.Monad.Error,
    module Control.Concurrent,
    module Data.Array,
    module Data.Bits,
    module Data.List,
    module Data.Either,
    module Data.Word,
    module Data.Ratio,
    module Data.Char,
    module Data.Tree,
    module Data.Maybe,
    module Data.Complex,
    module Data.Set,
    module Data.Map,
    module Data.IORef,
    module Debug.Trace,
    module Network,
    internalError,
    split,
    breakOnGlue,
    afterPrefix,
    decodeUTF8,
    encodeUTF8,
    forM,
    forM_,
    tryIO,
    combine,
) where

import UTF8
import Unicode
import Pugs.Cont
import Pugs.Embed
import Pugs.Compat
import RRegex
import RRegex.Syntax
import Data.Dynamic
import Data.Array (elems)
import Network
import System.Environment (getArgs, withArgs, getProgName, getEnv)
import System.Random hiding (split)
import System.Exit
import System.Time
import System.Cmd
import System.IO (
    Handle, stdin, stdout, hClose, hGetLine, hGetContents,
    openFile, hPutStr, hPutStrLn, IOMode(..), stderr, SeekMode(..),
    hSetBuffering, BufferMode(..), hIsTerminalDevice, hFlush, hPrint
    )
import System.IO.Unsafe
import System.IO.Error (ioeGetErrorString, isUserError)
import System.Directory
import Control.Exception (catchJust, errorCalls)
import Control.Monad.RWS
import Control.Monad.Error (MonadError(..))
import Control.Concurrent
import Data.Bits hiding (shift)
import Data.Maybe
import Data.Either
import Data.List (
    (\\), find, genericLength, insert, sortBy, intersperse,
    partition, group, sort, genericReplicate, isPrefixOf, isSuffixOf,
    genericTake, genericDrop, unfoldr, nub, nubBy, transpose
    )
import Data.Unique
import Data.Ratio
import Data.Word
import Data.Char (chr, ord, digitToInt)
import Data.Ratio
import Data.Complex
import Data.Tree
import Data.IORef
import Data.Set (Set)
import Data.Map (Map)
import Debug.Trace
import Pugs.Rule.Pos

-- Instances.
instance Show Unique where
    show = show . hashUnique
instance Show (a -> b) where
    show _ = "(->)"
instance Eq (a -> b) where
    _ == _ = False
instance Ord (a -> b) where
    compare _ _ = LT

internalError :: String -> a
internalError s = error $ 
    "Internal error:\n    " ++ s ++ "\nPlease file a bug report."

split :: (Eq a) => [a] -> [a] -> [[a]]
split []  _   = internalError "splitting by an empty list"
split sep str =
   case breakOnGlue sep str of
     Just (before, after) -> before : split sep after
     Nothing -> [str]

-- returns Nothing if the glue isn't there
breakOnGlue :: (Eq a) => [a] -> [a] -> Maybe ([a], [a])
breakOnGlue _    [] = Nothing
breakOnGlue glue list@(x:xs) =
   case afterPrefix glue list of
      Just rest -> Just ([], rest)
      Nothing -> case breakOnGlue glue xs of
                    Just (before, after) -> Just (x : before, after)
                    Nothing -> Nothing

afterPrefix :: (Eq a) => [a] -> [a] -> Maybe [a]
afterPrefix []     list = Just list
afterPrefix _      []   = Nothing  -- non-empty prefix of an empty list
afterPrefix (p:ps) (x:xs)
   | p == x = afterPrefix ps xs
   | otherwise = Nothing

encodeUTF8 :: String -> String
encodeUTF8 = map (chr . fromEnum) . encode

decodeUTF8 :: String -> String
decodeUTF8 str = fst $ decode bytes
    where
    bytes = map (toEnum . ord) str

forM :: (Monad m) => [a] -> (a -> m b) -> m [b]
forM = flip mapM

forM_ :: (Monad m) => [a] -> (a -> m b) -> m ()
forM_ = flip mapM_

tryIO :: (MonadIO m) => a -> IO a -> m a
tryIO err = liftIO . (`catch` (const $ return err))

combine = foldr (.) id
