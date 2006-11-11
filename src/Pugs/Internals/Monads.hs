{-# OPTIONS_GHC -fglasgow-exts -fno-warn-orphans -fno-full-laziness -fno-cse -fno-warn-deprecations -fallow-undecidable-instances -fallow-overlapping-instances -funbox-strict-fields -cpp #-}

module Pugs.Internals.Monads (
    internalError,
    traceM,
    warn,
    die,

    forM,
    forM_,
    combine,
    modifyTVar,
    inlinePerformIO,
    inlinePerformSTM,
    unsafePerformSTM,
    maybeM, finallyM,
    catchIO, evaluateIO
) where

import GHC.Base (realWorld#)
import Data.FunctorM
import Debug.Trace
import GHC.IOBase (IO(..))
import System.Exit
import System.IO (
    Handle, stdin, stdout, hClose, hGetLine, hGetChar, hGetContents,
    openFile, hSetBinaryMode, hPutStr, hPutStrLn, IOMode(..), stderr, SeekMode(..),
    hSetBuffering, BufferMode(..), hIsTerminalDevice, hFlush, hPrint, isEOF,
    hSeek, hTell, hIsOpen, hIsClosed, hIsReadable, hIsWritable, hIsSeekable,
    )
import System.IO.Unsafe
import System.IO.Error (ioeGetErrorString, isUserError)
import Control.Exception (Exception(..))
import Control.Concurrent.STM
import Control.Monad.RWS (MonadIO(..))
import qualified Control.Exception (catch, evaluate)

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

catchIO :: IO a -> (Control.Exception.Exception -> IO a) -> IO a
catchIO = Control.Exception.catch

evaluateIO :: a -> IO a
evaluateIO = Control.Exception.evaluate

{-# INLINE finallyM #-}
finallyM :: (Monad m) 
     => m a     -- ^ The actual action
     -> m b     -- ^ the finalizer
     -> m a     -- ^ Result of the actual action
finallyM ma mb = do
    r <- ma
    mb
    return r

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

