{-# OPTIONS_GHC -fglasgow-exts -fno-warn-orphans #-}

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
    module UTF8,
    module Unicode,
    module Pugs.Compat,
    module RRegex,
    module RRegex.Syntax,
    module Pugs.Rule.Pos,
    module Data.Dynamic,
    module Data.Unique,
    module Data.FunctorM,
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
    module System.Process,
    module System.Mem,
    module System.Mem.Weak,
    module Control.Monad.RWS,
    module Control.Monad.Error,
    module Control.Concurrent,
    module Control.Concurrent.STM,
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
    module Data.IntMap,
    module Debug.Trace,
    module Network,
    internalError,
    split,
    split_n,
    breakOnGlue,
    afterPrefix,
    decodeUTF8,
    encodeUTF8,
    forM,
    forM_,
    tryIO,
    combine,
    modifyTVar,
    unsafePerformSTM,
    possiblyFixOperatorName,
    maybeM,
    safeMode,
) where

import UTF8
import Unicode
import Pugs.Compat
import RRegex
import RRegex.Syntax
import Data.Dynamic
import Data.Array (elems)
import Network
import System.Environment (getArgs, withArgs, getProgName)
import System.Random hiding (split)
import System.Exit
import System.Time
import System.Cmd
import System.Process
import System.IO (
    Handle, stdin, stdout, hClose, hGetLine, hGetChar, hGetContents,
    openFile, hSetBinaryMode, hPutStr, hPutStrLn, IOMode(..), stderr, SeekMode(..),
    hSetBuffering, BufferMode(..), hIsTerminalDevice, hFlush, hPrint, isEOF, hSeek
    )
import System.IO.Unsafe
import System.IO.Error (ioeGetErrorString, isUserError)
import System.Directory
import System.Mem
import System.Mem.Weak
import Control.Exception (catchJust, errorCalls)
import Control.Monad.RWS
import Control.Monad.Error (MonadError(..))
import Control.Concurrent
import Control.Concurrent.STM
import Data.Bits hiding (shift)
import Data.Maybe
import Data.Either
import Data.FunctorM
import Data.List (
    (\\), find, genericLength, insert, sortBy, intersperse,
    partition, group, sort, genericReplicate, isPrefixOf, isSuffixOf,
    genericTake, genericDrop, unfoldr, nub, nubBy, transpose, delete
    )
import Data.Unique
import Data.Ratio
import Data.Word
import Data.Char (chr, ord, digitToInt)
import Data.Complex
import Data.Tree
import Data.Set (Set)
import Data.Map (Map)
import Data.IntMap (IntMap)
import Debug.Trace
import Pugs.Rule.Pos
-- import GHC.Conc (unsafeIOToSTM)

-- Instances.
instance Show Unique where
    show = show . hashUnique
instance (Typeable a, Typeable b) => Show (a -> b) where
    show _ = "(" ++ typA ++ " -> " ++ typB ++ ")"
        where
        typA = show $ typeOf (undefined :: a)
        typB = show $ typeOf (undefined :: b)
instance Eq (a -> b) where
    _ == _ = False
instance Ord (a -> b) where
    compare _ _ = LT
instance Eq Dynamic where
    _ == _ = False
instance Ord Dynamic where
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

split_n :: (Eq a) => [a] -> [a] -> Int -> [[a]]
split_n [] _ _ = internalError "splitting by an empty list"
split_n sep str n
   | n == 1 = [str]
   | otherwise =
   case breakOnGlue sep str of
       Just (before, after) -> before : split_n sep after (n-1)
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

{-|
Take a list of values, and a monad-producing function, and apply that function
to each element of the list. The resulting monads are combined into a single
monad producing a list of the resulting values.

(This is just @mapM@ with the arguments reversed.)
-}
forM :: (Monad m) 
     => [a]        -- ^ List of values to loop over
     -> (a -> m b) -- ^ The \'body\' of the for loop
     -> m [b]      -- ^ Monad containing a list of the results
forM = flip mapM

{-|
Take a list of values, and a monad-producing function, and apply that function
to each element of the list in sequence. The values produced by the monadic
function are discarded.

(This is just @mapM_@ with the arguments reversed.)
-}
forM_ :: (Monad m) 
      => [a]        -- ^ List of values to loop over
      -> (a -> m b) -- ^ The \'body\' of the for loop
      -> m ()
forM_ = flip mapM_

tryIO :: (MonadIO m) => a -> IO a -> m a
tryIO err = liftIO . (`catch` (const $ return err))

{-|
Compose a list of @(a -> a)@ transformer functions into a single chained
function, using @foldr@ via the @(.)@ operator.

Note that the transformations are applied to the eventual argument in 
right-to-left order.
-}
combine :: [a -> a] -- ^ List of transformer functions
        -> (a -> a) -- ^ The final combined transformer
combine = foldr (.) id

unsafePerformSTM :: STM a -> a
unsafePerformSTM = unsafePerformIO . atomically

{-|
Read an STM variable, apply some transformation function to it, and write the
transformed value back to the same variable.
-}
modifyTVar :: TVar a 
           -> (a -> a) 
           -> STM ()
modifyTVar var f = do
    x <- readTVar var
    writeTVar var (f x)

-- instance MonadIO STM where
--     liftIO = unsafeIOToSTM

{-|
Extract a @Maybe@ value from the first argument (a monad).

If it's a @Just@ (i.e. it contains a value), apply the second argument
(a monad-producing function) to it, and @return@ the contents of /that/ 
wrapped in a @Just@.

Otherwise, merely @return Nothing@.

(Strictly speaking, this function can operate with any @FunctorM@, not just
@Maybe@, but it helps to have a concrete example to explain things.)
-}
maybeM :: (FunctorM f, Monad m) 
       => m (f a)    -- ^ A @Maybe@ value encapsulated in a monad
       -> (a -> m b) -- ^ Action to perform on the first arg /if/ it contains
                     --     a value
       -> m (f b)    -- ^ Monad containing (@Just@ /result/) or @Nothing@
maybeM f m = fmapM m =<< f

{-|
Transform an operator name, for example @&infix:\<+\>@ or @&prefix:«[+]»@, 
into its internal name (@&infix:+@ and @&prefix:[+]@ respectively).
-}
possiblyFixOperatorName :: String -> String
possiblyFixOperatorName name
    -- It doesn't matter if we lookup &foo or &*foo.
    | ('&':'*':rest) <- name = "&*" ++ fixName' rest
    | ('&':rest)     <- name = "&"  ++ fixName' rest
    | otherwise      = name
    where
    -- We've to strip the <>s for &infix:<...>, &prefix:<...>, and
    -- &postfix:<...>.
    -- The other &...:<...> things aren't that simple (e.g. circumfix.).
    fixName' ('i':'n':'f':'i':'x':':':rest)         = "infix:"   ++ dropBrackets rest
    fixName' ('p':'r':'e':'f':'i':'x':':':rest)     = "prefix:"  ++ dropBrackets rest
    fixName' ('p':'o':'s':'t':'f':'i':'x':':':rest) = "postfix:" ++ dropBrackets rest
    fixName' x                                      = x
    -- We have to make sure that the last character(s) match the first one(s),
    -- otherwise 4 <= 4 will stop working.
    -- Kludge. <=> is ambigious.
    dropBrackets "<=>" = "<=>"
    -- «bar» --> bar
    dropBrackets ('\171':(rest@(_:_)))    = if (last rest) == '\187' then init rest else '\171':rest
    -- <<bar>> --> bar
    dropBrackets ('<':'<':(rest@(_:_:_))) = if (last rest) == '>' && (last . init $ rest) == '>' then init . init $ rest else "<<" ++ rest
    -- <bar> --> bar
    dropBrackets ('<':(rest@(_:_)))       = if (last rest) == '>' then init rest else '<':rest
    dropBrackets x                        = x

{-|
Returns @True@ if the environment variable @PUGS_SAFEMODE@ is set to a
true value. Most IO primitives are disabled under safe mode.
-}
{-# NOINLINE safeMode #-}
safeMode :: Bool
safeMode = case (unsafePerformIO $ getEnv "PUGS_SAFEMODE") of
    Nothing     -> False
    Just ""     -> False
    Just "0"    -> False
    _           -> True
