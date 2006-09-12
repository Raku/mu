{-# OPTIONS_GHC -fglasgow-exts -fno-warn-orphans -fno-full-laziness -fno-cse -fno-warn-deprecations -fallow-undecidable-instances -fallow-overlapping-instances -funbox-strict-fields -cpp #-}

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
    module Control.Concurrent,
    module Control.Concurrent.STM,
    module Control.Exception,
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
    module Data.FunctorM,
    module Data.IntMap,
    module Data.IORef,
    module Data.List,
    module Data.Map,
    module Data.Maybe,
    module Data.Ratio,
    module Data.Set,
    module Data.Seq,
    module Data.Tree,
    module Data.Unique,
    module Data.Word,
    module Debug.Trace,
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
    module System.Time,
    (:>:)(..),
    (:<:)(..),
    internalError,
    split,
    split_n,
    breakOnGlue,
    afterPrefix,
    decodeUTF8,
    encodeUTF8,
    fromTypeable,
    forM,
    forM_,
    combine,
    modifyTVar,
    inlinePerformIO,
    inlinePerformSTM,
    unsafePerformSTM,
    maybeM,
    safeMode,
    traceM,
    warn,
    die,

    catchIO, evaluateIO,

    _GlobalFinalizer,
    unsafeIOToSTM,
    ID(..), bufToID,

    __, (+++), nullID,
) where

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
import Control.Monad.RWS (MonadIO(..), MonadReader(..), MonadState(..), MonadWriter(..), MonadTrans(..), asks, ReaderT(..), WriterT(..), when, join, liftM, filterM, modify, unless, gets, foldM, guard, liftM2, liftM3, fix, mplus, mappend, mzero, mconcat, msum, censor)
import Control.Monad.Identity (Identity(..))
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
import Data.Seq (Seq, singleton)
import Data.IntMap (IntMap)
import Debug.Trace
import GHC.Base (realWorld#)
import GHC.IOBase (IO(..))
import GHC.Conc (unsafeIOToSTM)
import GHC.Exts (unsafeCoerce#, Word(W#), Word#)
import qualified Data.Seq as Seq

import qualified Judy.StrMap as H
import qualified Judy.CollectionsM as C
import qualified Data.ByteString.Char8 as Char8
import qualified Foreign as Foreign
import qualified Control.Exception (catch, evaluate)

--
-- Nominal subtyping relationship with widening cast.
-- 
-- The function "cast" is injective: for distinct values of "b",
-- it must produce distinct values of "a".
--
-- Also, it must work for all values of type "b".
-- 
class ((:>:) a) b where
    {-# SPECIALISE cast :: ID -> ID #-}
    {-# SPECIALISE cast :: ByteString -> ByteString #-}
    {-# SPECIALISE cast :: String -> String #-}
    {-# SPECIALISE cast :: ByteString -> ByteString #-}
    {-# SPECIALISE cast :: String -> String #-}
    {-# SPECIALISE cast :: ID -> ByteString #-}
    {-# SPECIALISE cast :: ID -> String #-}
    {-# SPECIALISE cast :: ByteString -> ID #-}
    {-# SPECIALISE cast :: String -> ID #-}
    cast :: b -> a

class ((:<:) a) b where
    castBack :: a -> b

instance (b :<: a) => (:>:) a b where
    cast = castBack

instance (:<:) a a where castBack = id

instance ((:>:) [a]) (Seq a) where cast = Seq.toList
instance ((:<:) [a]) (Seq a) where castBack = Seq.fromList

-- "return . cast" can be written as "cast"
instance (Monad m, (a :>: b)) => ((:>:) (m a)) b where cast = return . cast

-- "fmap cast" can be written as "cast"
instance (Functor f, (a :>: b)) => ((:>:) (f a)) (f b) where cast = fmap cast

fromTypeable :: forall m a b. (Monad m, Typeable a, Typeable b) => a -> m b
fromTypeable x = case Typeable.cast x of
    Just y -> return y
    _      -> fail $ "Cannot cast from " ++ (show $ typeOf x) ++ " to " ++ (show $ typeOf (undefined :: b))

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

internalError :: String -> a
internalError s = error $
    "Internal error:\n    " ++ s ++ "\nPlease file a bug report."

die :: (MonadIO m, Show a) => String -> a -> m b
die x y = do
    warn x y
    liftIO $ exitFailure

warn :: (MonadIO m, Show a) => String -> a -> m ()
warn str val = liftIO $ do
    hPutStrLn stderr $ "*** " ++ str ++ ":\n    " ++ show val

-- | This is just @Debug.Trace.trace@, but allows for cleaner code in do blocks.
traceM :: Monad m => String -> m ()
traceM s = trace s $ return ()

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

{-# INLINE decodeUTF8 #-}
decodeUTF8 :: String -> String
decodeUTF8 [] = []
decodeUTF8 (c:cs)
    | c < '\x80'
    = let rest = decodeUTF8 cs
       in seq rest
          (c:rest)
decodeUTF8 (c:d:cs)
    | '\xC0' <= c, c <= '\xDF'
    , '\x80' <= d, d <= '\xBF'
    = let rest = decodeUTF8 cs
       in seq rest
          ( toEnum ( (fromEnum c `mod` 0x20) * 0x40
                   + fromEnum d `mod` 0x40
                   )
          : rest
          )
decodeUTF8 (c:d:e:cs)
    | '\xE0' <= c, c <= '\xEF'
    , '\x80' <= d, d <= '\xBF'
    , '\x80' <= e, e <= '\xBF'
    = let rest = decodeUTF8 cs
       in seq rest
          ( toEnum ( (fromEnum c `mod` 0x10 * 0x1000)
                   + (fromEnum d `mod` 0x40) * 0x40
                   + fromEnum e `mod` 0x40
                   )
          : rest
          )
decodeUTF8 (c:d:e:f:cs)
    | '\xF0' <= c, c <= '\xF7'
    , '\x80' <= d, d <= '\xBF'
    , '\x80' <= e, e <= '\xBF'
    , '\x80' <= f, f <= '\xBF'
    = let rest = decodeUTF8 cs
       in seq rest
          ( toEnum ( (fromEnum c `mod` 0x10 * 0x40000)
                   + (fromEnum d `mod` 0x40) * 0x1000
                   + (fromEnum e `mod` 0x40) * 0x40
                   + fromEnum f `mod` 0x40
                   )
          : rest
          )
decodeUTF8 (x:xs) = trace ("decodeUTF8: bad data: " ++ show x) (x:decodeUTF8 xs)

{-# INLINE encodeUTF8 #-}
encodeUTF8 :: String -> String
encodeUTF8 [] = []
-- In the \0 case, we diverge from the Unicode standard to remove any trace
-- of embedded nulls in our bytestrings, to allow the use of Judy.StrMap
-- and to make passing CString around easier.  See Java for the same treatment:
-- http://java.sun.com/j2se/1.5.0/docs/api/java/io/DataInput.html#modified-utf-8
encodeUTF8 ('\0':cs)
    = let rest = encodeUTF8 cs
       in seq rest
          ('\xC0':'\x80':rest)
encodeUTF8 (c:cs)
    | c < '\x80'
    = let rest = encodeUTF8 cs
       in seq rest
          (c:rest)
    | c < '\x800'
    = let i     = fromEnum c
          rest  = encodeUTF8 cs
       in seq rest
          ( toEnum (0xC0 + i `div` 0x40)
          : toEnum (0x80 + i `mod` 0x40)
          : rest
          )
    | c < '\x10000'
    = let i     = fromEnum c
          rest  = encodeUTF8 cs
       in seq rest
          ( toEnum (0xE0 + i `div` 0x1000)
          : toEnum (0x80 + (i `div` 0x40) `mod` 0x40)
          : toEnum (0x80 + i `mod` 0x40)
          : rest
          )
    | otherwise
    = let i     = fromEnum c
          rest  = encodeUTF8 cs
       in seq rest
          ( toEnum (0xF0 + i `div` 0x40000)
          : toEnum (0x80 + (i `div` 0x1000) `mod` 0x40)
          : toEnum (0x80 + (i `div` 0x40) `mod` 0x40)
          : toEnum (0x80 + i `mod` 0x40)
          : rest
          )

catchIO :: IO a -> (Control.Exception.Exception -> IO a) -> IO a
catchIO = Control.Exception.catch

evaluateIO :: a -> IO a
evaluateIO = Control.Exception.evaluate

-- On GHC 6.6 we actually want to use the builtin forM and forM_ in Control.Monad

{-|
Take a list of values, and a monad-producing function, and apply that function
to each element of the list. The resulting monads are combined into a single
monad producing a list of the resulting values.

(This is just @mapM@ with the arguments reversed.)
-}
{-# INLINE forM #-}
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
{-# INLINE forM_ #-}
forM_ :: (Monad m) 
      => [a]        -- ^ List of values to loop over
      -> (a -> m b) -- ^ The \'body\' of the for loop
      -> m ()
forM_ = flip mapM_

{-|
Compose a list of @(a -> a)@ transformer functions into a single chained
function, using @foldr@ via the @(.)@ operator.

Note that the transformations are applied to the eventual argument in 
right-to-left order.
-}
combine :: [a -> a] -- ^ List of transformer functions
        -> (a -> a) -- ^ The final combined transformer
combine = foldr (.) id

{-# INLINE inlinePerformIO #-}
inlinePerformIO :: IO a -> a
inlinePerformIO (IO m) = case m realWorld# of (# _, r #) -> r

{-# INLINE inlinePerformSTM #-}
inlinePerformSTM :: STM a -> a
inlinePerformSTM m = inlinePerformIO (atomically m)

{-# NOINLINE unsafePerformSTM #-}
unsafePerformSTM :: STM a -> a
unsafePerformSTM m = unsafePerformIO (atomically m)

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
Returns @True@ if the environment variable @PUGS_SAFEMODE@ is set to a
true value. Most IO primitives are disabled under safe mode.
-}
safeMode :: Bool
safeMode = case (inlinePerformIO $ getEnv "PUGS_SAFEMODE") of
    Nothing     -> False
    Just ""     -> False
    Just "0"    -> False
    _           -> True

{-# NOINLINE _GlobalFinalizer #-}
_GlobalFinalizer :: IORef (IO ())
_GlobalFinalizer = unsafePerformIO $ newIORef (return ())

-- XXX - Under GHCI, our global _BufToID table could be refreshed into
--       nonexistence, so we need to compare IDs based on the actual buffer,
--       not its unique key.
data ID = MkID
#ifdef PUGS_UNDER_GHCI
    { idBuf :: !ByteString, idKey :: !Int }
#else
    { idKey :: !Int, idBuf :: !ByteString }
#endif
    deriving (Typeable, Data)

instance Eq ID where
    MkID x _ == MkID y _ = x == y
    MkID x _ /= MkID y _ = x /= y

instance Ord ID where
    compare (MkID x _) (MkID y _) = compare x y
    MkID x _ <= MkID y _ = x <= y
    MkID x _ >= MkID y _ = x >= y
    MkID x _ < MkID y _ = x < y
    MkID x _ > MkID y _ = x > y

instance Show ID where
    showsPrec x MkID{ idBuf = buf } = showsPrec x buf

instance Read ID where
    readsPrec p s = [ (unsafePerformIO (bufToID (Char8.pack x)), y) | (x, y) <- readsPrec p s]

instance ((:>:) String) ByteString where
    cast = decodeUTF8 . Char8.unpack
instance ((:<:) String) ByteString where
    castBack = Char8.pack . encodeUTF8

{-# NOINLINE nullID #-}
nullID :: ID
nullID = cast ""

{-# INLINE __ #-}
__ :: String -> ByteString
__ = Char8.pack

{-# INLINE (+++) #-}
(+++) :: ByteString -> ByteString -> ByteString
(+++) = Char8.append

{-# NOINLINE _BufToID #-}
_BufToID :: H.StrMap ByteString ID
_BufToID = unsafePerformIO C.new

{-# NOINLINE _ID_count #-}
_ID_count :: Foreign.Ptr Int
_ID_count = unsafePerformIO (Foreign.new 1)

instance ((:>:) ID) String where
    cast str = let i = unsafePerformIO (bufToID (cast str)) in idKey `seq` i

instance ((:>:) String) ID where
    cast = cast . idBuf

instance ((:<:) ID) ByteString where
    castBack = idBuf

instance ((:<:) ByteString) ID where
    castBack buf = let i = unsafePerformIO (bufToID buf) in idKey i `seq` i

{-# NOINLINE bufToID #-}
bufToID :: ByteString -> IO ID
bufToID buf = do
    a'      <- C.lookup buf _BufToID
    case a' of
        Just a  -> do
            -- hPrint stderr ("HIT", buf, W# (unsafeCoerce# _BufToID), W# (unsafeCoerce# cache))
            return a
        _       -> do
            i <- Foreign.peek _ID_count
            -- hPrint stderr ("MISS", buf, W# (unsafeCoerce# _BufToID), W# (unsafeCoerce# cache), i)
            Foreign.poke _ID_count (succ i)
            let a = MkID{ idKey = i, idBuf = buf }
            C.insert buf a _BufToID
            return a

