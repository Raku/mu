{-# LANGUAGE ForeignFunctionInterface #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) 2006-2008 Duncan Coutts
-- License     :  BSD-style
--
-- Maintainer  :  duncan@haskell.org
-- Stability   :  provisional
-- Portability :  portable (H98 + FFI)
--
-- Zlib wrapper layer
--
-----------------------------------------------------------------------------
module Codec.Compression.Zlib.Stream (

  -- * The Zlib state monad
  Stream,
  run,
  unsafeInterleave,
  unsafeLiftIO,
  finalise,

  -- * Initialisation
  deflateInit, 
  inflateInit,

  -- ** Initialisation parameters
  Format(..),
  CompressionLevel(..),
  Method(..),
  WindowBits(..),
  MemoryLevel(..),
  CompressionStrategy(..),

  -- * The buisness
  deflate,
  inflate,
  Status(..),
  Flush(..),

  -- * Buffer management
  -- ** Input buffer
  pushInputBuffer,
  inputBufferEmpty,

  -- ** Output buffer
  pushOutputBuffer,
  popOutputBuffer,
  outputBufferBytesAvailable,
  outputBufferSpaceRemaining,
  outputBufferFull,

  -- * Debugging
  consistencyCheck,
  dump,
  trace,

  ) where

import Foreign
         ( Word8, Ptr, nullPtr, plusPtr, peekByteOff, pokeByteOff, mallocBytes
         , ForeignPtr, FinalizerPtr, newForeignPtr_, addForeignPtrFinalizer
	 , finalizeForeignPtr, withForeignPtr, touchForeignPtr
	 , unsafeForeignPtrToPtr, unsafePerformIO )
import Foreign.C
         ( CInt, CUInt, CChar, CString, withCAString, peekCAString )
#ifdef BYTESTRING_IN_BASE
import Data.ByteString.Base (nullForeignPtr)
#else
import Data.ByteString.Internal (nullForeignPtr)
#endif
import System.IO.Unsafe (unsafeInterleaveIO)
import System.IO (hPutStrLn, stderr)
import Control.Monad (liftM)
import Control.Exception (assert)

import Prelude hiding (length)

#include "zlib.h"


pushInputBuffer :: ForeignPtr Word8 -> Int -> Int -> Stream ()
pushInputBuffer inBuf' offset length = do

  -- must not push a new input buffer if the last one is not used up
  inAvail <- getInAvail
  assert (inAvail == 0) $ return ()

  -- Now that we're setting a new input buffer, we can be sure that zlib no
  -- longer has a reference to the old one. Therefore this is the last point
  -- at which the old buffer had to be retained. It's safe to release now.
  inBuf <- getInBuf 
  unsafeLiftIO $ touchForeignPtr inBuf    

  -- now set the available input buffer ptr and length
  setInBuf   inBuf'
  setInAvail length
  setInNext  (unsafeForeignPtrToPtr inBuf' `plusPtr` offset)
  -- Note the 'unsafe'. We are passing the raw ptr inside inBuf' to zlib.
  -- To make this safe we need to hold on to the ForeignPtr for at least as
  -- long as zlib is using the underlying raw ptr.


inputBufferEmpty :: Stream Bool
inputBufferEmpty = getInAvail >>= return . (==0)


pushOutputBuffer :: ForeignPtr Word8 -> Int -> Int -> Stream ()
pushOutputBuffer outBuf' offset length = do

  --must not push a new buffer if there is still data in the old one
  outAvail <- getOutAvail
  assert (outAvail == 0) $ return ()
  -- Note that there may still be free space in the output buffer, that's ok,
  -- you might not want to bother completely filling the output buffer say if
  -- there's only a few free bytes left.

  outBuf <- getOutBuf
  unsafeLiftIO $ touchForeignPtr outBuf

  -- now set the available input buffer ptr and length
  setOutBuf  outBuf'
  setOutFree length
  setOutNext (unsafeForeignPtrToPtr outBuf' `plusPtr` offset)

  setOutOffset offset
  setOutAvail  0


-- get that part of the output buffer that is currently full
-- (might be 0, use outputBufferBytesAvailable to check)
-- this may leave some space remaining in the buffer, use
-- outputBufferSpaceRemaining to check.
popOutputBuffer :: Stream (ForeignPtr Word8, Int, Int)
popOutputBuffer = do

  outBuf    <- getOutBuf
  outOffset <- getOutOffset
  outAvail  <- getOutAvail

  -- there really should be something to pop, otherwise it's silly
  assert (outAvail > 0) $ return ()

  setOutOffset (outOffset + outAvail)
  setOutAvail  0

  return (outBuf, outOffset, outAvail)


-- this is the number of bytes available in the output buffer
outputBufferBytesAvailable :: Stream Int
outputBufferBytesAvailable = getOutAvail


-- you needen't get all the output immediately, you can continue until
-- there is no more output space available, this tells you that amount
outputBufferSpaceRemaining :: Stream Int
outputBufferSpaceRemaining = getOutFree


-- you only need to supply a new buffer when there is no more output buffer
-- space remaining
outputBufferFull :: Stream Bool
outputBufferFull = getOutFree >>= return . (==0)


-- you can only run this when the output buffer is not empty
-- you can run it when the input buffer is empty but it doesn't do anything
-- after running deflate either the output buffer will be full
-- or the input buffer will be empty (or both)
deflate :: Flush -> Stream Status
deflate flush = do

  outFree <- getOutFree

  -- deflate needs free space in the output buffer
  assert (outFree > 0) $ return ()

  result <- deflate_ flush
  outFree' <- getOutFree
    
  -- number of bytes of extra output there is available as a result of
  -- the call to deflate:
  let outExtra = outFree - outFree'
  
  outAvail <- getOutAvail
  setOutAvail (outAvail + outExtra)
  return result


inflate :: Flush -> Stream Status
inflate flush = do

  outFree <- getOutFree

  -- inflate needs free space in the output buffer
  assert (outFree > 0) $ return ()

  result <- inflate_ flush
  outFree' <- getOutFree

  -- number of bytes of extra output there is available as a result of
  -- the call to inflate:
  let outExtra = outFree - outFree'

  outAvail <- getOutAvail
  setOutAvail (outAvail + outExtra)
  return result


----------------------------
-- Stream monad
--

newtype Stream a = Z {
    unZ :: ForeignPtr StreamState
        -> ForeignPtr Word8
        -> ForeignPtr Word8
        -> Int -> Int
        -> IO (ForeignPtr Word8
              ,ForeignPtr Word8
              ,Int, Int, a)
  }

instance Monad Stream where
  (>>=)  = thenZ
--  m >>= f = (m `thenZ` \a -> consistencyCheck `thenZ_` returnZ a) `thenZ` f
  (>>)   = thenZ_
  return = returnZ
  fail   = (finalise >>) . failZ

returnZ :: a -> Stream a
returnZ a = Z $ \_ inBuf outBuf outOffset outLength ->
                  return (inBuf, outBuf, outOffset, outLength, a)
{-# INLINE returnZ #-}

thenZ :: Stream a -> (a -> Stream b) -> Stream b
thenZ (Z m) f =
  Z $ \stream inBuf outBuf outOffset outLength ->
    m stream inBuf outBuf outOffset outLength >>=
      \(inBuf', outBuf', outOffset', outLength', a) ->
        unZ (f a) stream inBuf' outBuf' outOffset' outLength'
{-# INLINE thenZ #-}

thenZ_ :: Stream a -> Stream b -> Stream b
thenZ_ (Z m) f =
  Z $ \stream inBuf outBuf outOffset outLength ->
    m stream inBuf outBuf outOffset outLength >>=
      \(inBuf', outBuf', outOffset', outLength', _) ->
        unZ f stream inBuf' outBuf' outOffset' outLength'
{-# INLINE thenZ_ #-}

failZ :: String -> Stream a
failZ msg = Z (\_ _ _ _ _ -> fail ("Codec.Compression.Zlib: " ++ msg))

{-# NOINLINE run #-}
run :: Stream a -> a
run (Z m) = unsafePerformIO $ do
  ptr <- mallocBytes (#{const sizeof(z_stream)})
  #{poke z_stream, msg}       ptr nullPtr
  #{poke z_stream, zalloc}    ptr nullPtr
  #{poke z_stream, zfree}     ptr nullPtr
  #{poke z_stream, opaque}    ptr nullPtr
  #{poke z_stream, next_in}   ptr nullPtr
  #{poke z_stream, next_out}  ptr nullPtr
  #{poke z_stream, avail_in}  ptr (0 :: CUInt)
  #{poke z_stream, avail_out} ptr (0 :: CUInt)
  stream <- newForeignPtr_ ptr
  (_,_,_,_,a) <- m stream nullForeignPtr nullForeignPtr 0 0
  return a

unsafeLiftIO :: IO a -> Stream a
unsafeLiftIO m = Z $ \_stream inBuf outBuf outOffset outLength -> do
  a <- m
  return (inBuf, outBuf, outOffset, outLength, a)

-- It's unsafe because we discard the values here, so if you mutate anything
-- between running this and forcing the result then you'll get an inconsistent
-- stream state.
unsafeInterleave :: Stream a -> Stream a
unsafeInterleave (Z m) = Z $ \stream inBuf outBuf outOffset outLength -> do
  res <- unsafeInterleaveIO (m stream inBuf outBuf outOffset outLength)
  let select (_,_,_,_,a) = a
  return (inBuf, outBuf, outOffset, outLength, select res)

getStreamState :: Stream (ForeignPtr StreamState)
getStreamState = Z $ \stream inBuf outBuf outOffset outLength -> do
  return (inBuf, outBuf, outOffset, outLength, stream)

getInBuf :: Stream (ForeignPtr Word8)
getInBuf = Z $ \_stream inBuf outBuf outOffset outLength -> do
  return (inBuf, outBuf, outOffset, outLength, inBuf)

getOutBuf :: Stream (ForeignPtr Word8)
getOutBuf = Z $ \_stream inBuf outBuf outOffset outLength -> do
  return (inBuf, outBuf, outOffset, outLength, outBuf)

getOutOffset :: Stream Int
getOutOffset = Z $ \_stream inBuf outBuf outOffset outLength -> do
  return (inBuf, outBuf, outOffset, outLength, outOffset)

getOutAvail :: Stream Int
getOutAvail = Z $ \_stream inBuf outBuf outOffset outLength -> do
  return (inBuf, outBuf, outOffset, outLength, outLength)

setInBuf :: ForeignPtr Word8 -> Stream ()
setInBuf inBuf = Z $ \_stream _ outBuf outOffset outLength -> do
  return (inBuf, outBuf, outOffset, outLength, ())

setOutBuf :: ForeignPtr Word8 -> Stream ()
setOutBuf outBuf = Z $ \_stream inBuf _ outOffset outLength -> do
  return (inBuf, outBuf, outOffset, outLength, ())

setOutOffset :: Int -> Stream ()
setOutOffset outOffset = Z $ \_stream inBuf outBuf _ outLength -> do
  return (inBuf, outBuf, outOffset, outLength, ())

setOutAvail :: Int -> Stream ()
setOutAvail outLength = Z $ \_stream inBuf outBuf outOffset _ -> do
  return (inBuf, outBuf, outOffset, outLength, ())

----------------------------
-- Debug stuff
--

trace :: String -> Stream ()
trace = unsafeLiftIO . hPutStrLn stderr

dump :: Stream ()
dump = do
  inNext  <- getInNext
  inAvail <- getInAvail

  outNext <- getOutNext
  outFree <- getOutFree
  outAvail <- getOutAvail
  outOffset <- getOutOffset

  unsafeLiftIO $ hPutStrLn stderr $
    "Stream {\n" ++
    "  inNext    = " ++ show inNext    ++ ",\n" ++
    "  inAvail   = " ++ show inAvail   ++ ",\n" ++
    "\n" ++
    "  outNext   = " ++ show outNext   ++ ",\n" ++
    "  outFree   = " ++ show outFree   ++ ",\n" ++
    "  outAvail  = " ++ show outAvail  ++ ",\n" ++
    "  outOffset = " ++ show outOffset ++ "\n"  ++
    "}"

  consistencyCheck

consistencyCheck :: Stream ()
consistencyCheck = do

  outBuf    <- getOutBuf
  outOffset <- getOutOffset
  outAvail  <- getOutAvail
  outNext   <- getOutNext

  let outBufPtr = unsafeForeignPtrToPtr outBuf

  assert (outBufPtr `plusPtr` (outOffset + outAvail) == outNext) $ return ()


----------------------------
-- zlib wrapper layer
--

data Status =
    Ok
  | StreamEnd
  | NeedDict
  | BufferError -- ^ No progress was possible or there was not enough room in
                --   the output buffer when 'Finish' is used. Note that
                --   'BuferError' is not fatal, and 'inflate' can be called
                --   again with more input and more output space to continue.

toStatus :: CInt -> Status
toStatus (#{const Z_OK})         = Ok
toStatus (#{const Z_STREAM_END}) = StreamEnd
toStatus (#{const Z_NEED_DICT})  = NeedDict
toStatus (#{const Z_BUF_ERROR})  = BufferError
toStatus other = error ("unexpected zlib status: " ++ show other)

failIfError :: CInt -> Stream ()
failIfError errno
  | errno >= 0
 || errno == #{const Z_BUF_ERROR} = return ()
  | otherwise                     = fail =<< getErrorMessage errno

getErrorMessage :: CInt -> Stream String
getErrorMessage errno = do
  msgPtr <- withStreamPtr (#{peek z_stream, msg})
  if msgPtr /= nullPtr
    then unsafeLiftIO (peekCAString msgPtr)
    else return $ case errno of
      #{const Z_ERRNO}         -> "file error"
      #{const Z_STREAM_ERROR}  -> "stream error"
      #{const Z_DATA_ERROR}    -> "data error"
      #{const Z_MEM_ERROR}     -> "insufficient memory"
      #{const Z_VERSION_ERROR} -> "incompatible version"
      _                        -> "unknown error"

data Flush =
    NoFlush
  | SyncFlush
  | FullFlush
  | Finish
--  | Block -- only available in zlib 1.2 and later, uncomment if you need it.

fromFlush :: Flush -> CInt
fromFlush NoFlush   = #{const Z_NO_FLUSH}
fromFlush SyncFlush = #{const Z_SYNC_FLUSH}
fromFlush FullFlush = #{const Z_FULL_FLUSH}
fromFlush Finish    = #{const Z_FINISH}
--  fromFlush Block     = #{const Z_BLOCK}

-- | The format used for compression or decompression. There are three
-- variations.
--
data Format =
    GZip -- ^ The gzip format uses a header with a checksum and some optional
         -- meta-data about the compressed file. It is intended primarily for
         -- compressing individual files but is also sometimes used for network
         -- protocols such as HTTP. The format is described in detail in RFC
         -- #1952 <http://www.ietf.org/rfc/rfc1952.txt>

  | Zlib -- | The zlib format uses a minimal header with a checksum but no
         -- other meta-data. It is especially designed for use in network
         -- protocols. The format is described in detail in RFC #1950
         -- <http://www.ietf.org/rfc/rfc1950.txt>

  | Raw  -- | The \'raw\' format is just the compressed data stream without any
         -- additional header, meta-data or data-integrity checksum. The format
         -- is described in detail in RFC #1951
         -- <http://www.ietf.org/rfc/rfc1951.txt>

  | GZipOrZlib -- ^ This is not a format as such. It enabled zlib or gzip
               -- decoding with automatic header detection. This only makes
               -- sense for decompression.
  deriving Eq

-- | The compression method
--
data Method = Deflated -- ^ \'Deflate\' is the only method supported in this
                       -- version of zlib. Indeed it is likely to be the only
                       -- method that ever will be supported.

fromMethod :: Method -> CInt
fromMethod Deflated = #{const Z_DEFLATED}

-- | The compression level parameter controls the amount of compression. This
-- is a trade-off between the amount of compression and the time required to do
-- the compression.
--
data CompressionLevel = 
    DefaultCompression   -- ^ The default compression level is 6 (that is, 
                         --   biased towards higher compression at expense of
			 -- speed).
  | NoCompression        -- ^ No compression, just a block copy.
  | BestSpeed            -- ^ The fastest compression method (less compression) 
  | BestCompression      -- ^ The slowest compression method (best compression).
  | CompressionLevel Int -- ^ A specific compression level between 1 and 9.

fromCompressionLevel :: CompressionLevel -> CInt
fromCompressionLevel DefaultCompression   = -1
fromCompressionLevel NoCompression        = 0
fromCompressionLevel BestSpeed            = 1
fromCompressionLevel BestCompression      = 9
fromCompressionLevel (CompressionLevel n)
           | n >= 1 && n <= 9 = fromIntegral n
           | otherwise        = error "CompressLevel must be in the range 1..9"

-- | This specifies the size of the compression window. Larger values of this
-- parameter result in better compression at the expense of higher memory
-- usage.
--
-- The compression window size is the value of the the window bits raised to
-- the power 2. The window bits must be in the range @8..15@ which corresponds
-- to compression window sizes of 256b to 32Kb. The default is 15 which is also
-- the maximum size.
--
-- The total amount of memory used depends on the window bits and the
-- 'MemoryLevel'. See the 'MemoryLevel' for the details.
--
data WindowBits = DefaultWindowBits
                | WindowBits Int

windowBits :: Format -> WindowBits-> CInt
windowBits format bits = (formatModifier format) (checkWindowBits bits)
  where checkWindowBits DefaultWindowBits = 15
        checkWindowBits (WindowBits n)
          | n >= 8 && n <= 15 = fromIntegral n
          | otherwise         = error "WindowBits must be in the range 8..15"
        formatModifier Zlib       = id
        formatModifier GZip       = (+16)
        formatModifier GZipOrZlib = (+32)
        formatModifier Raw        = negate

-- | The 'MemoryLevel' parameter specifies how much memory should be allocated
-- for the internal compression state. It is a tradoff between memory usage,
-- compression ratio and compression speed. Using more memory allows faster
-- compression and a better compression ratio.
--
-- The total amount of memory used for compression depends on the 'WindowBits'
-- and the 'MemoryLevel'. For decompression it depends only on the
-- 'WindowBits'. The totals are given by the functions:
--
-- > compressTotal windowBits memLevel = 4 * 2^windowBits + 512 * 2^memLevel
-- > decompressTotal windowBits = 2^windowBits
--
-- For example, for compression with the default @windowBits = 15@ and
-- @memLevel = 8@ uses @256Kb@. So for example a network server with 100
-- concurrent compressed streams would use @25Mb@. The memory per stream can be
-- halved (at the cost of somewhat degraded and slower compressionby) by
-- reducing the @windowBits@ and @memLevel@ by one.
--
-- Decompression takes less memory, the default @windowBits = 15@ corresponds
-- to just @32Kb@.
--
data MemoryLevel =
    DefaultMemoryLevel -- ^ The default. (Equivalent to @'MemoryLevel' 8@)
  | MinMemoryLevel     -- ^ Use minimum memory. This is slow and reduces the
                       --   compression ratio. (Equivalent to @'MemoryLevel' 1@)
  | MaxMemoryLevel     -- ^ Use maximum memory for optimal compression speed.
                       --   (Equivalent to @'MemoryLevel' 9@)
  | MemoryLevel Int    -- ^ Use a specific level in the range @1..9@

fromMemoryLevel :: MemoryLevel -> CInt
fromMemoryLevel DefaultMemoryLevel = 8
fromMemoryLevel MinMemoryLevel     = 1
fromMemoryLevel MaxMemoryLevel     = 9
fromMemoryLevel (MemoryLevel n)
         | n >= 1 && n <= 9 = fromIntegral n
         | otherwise        = error "MemoryLevel must be in the range 1..9"


-- | The strategy parameter is used to tune the compression algorithm.
--
-- The strategy parameter only affects the compression ratio but not the
-- correctness of the compressed output even if it is not set appropriately.
--
data CompressionStrategy =
    DefaultStrategy -- ^ Use the 'DefaultStrategy' for normal data.
  | Filtered        -- ^ Use 'Filtered' for data produced by a filter (or
                    --   predictor). Filtered data consists mostly of small
                    --   values with a somewhat random distribution. In this
                    --   case, the compression algorithm is tuned to compress
                    --   them better. The effect of Z_FILTERED is to force more
                    --   Huffman coding and less string matching; it is
                    --   somewhat intermediate between 'DefaultStrategy' and
                    --   'HuffmanOnly'. 
  | HuffmanOnly     -- ^ Use 'HuffmanOnly' to force Huffman encoding only (no
                    --   string match). 

{-
-- -- only available in zlib 1.2 and later, uncomment if you need it.
  | RLE             -- ^ Use 'RLE' to limit match distances to one (run-length
                    --   encoding). 'RLE' is designed to be almost as fast as
                    --   'HuffmanOnly', but give better compression for PNG
                    --   image data.
  | Fixed           -- ^ 'Fixed' prevents the use of dynamic Huffman codes,
                    --   allowing for a simpler decoder for special applications.
-}

fromCompressionStrategy :: CompressionStrategy -> CInt
fromCompressionStrategy DefaultStrategy = #{const Z_DEFAULT_STRATEGY}
fromCompressionStrategy Filtered        = #{const Z_FILTERED}
fromCompressionStrategy HuffmanOnly     = #{const Z_HUFFMAN_ONLY}
--fromCompressionStrategy RLE             = #{const Z_RLE}
--fromCompressionStrategy Fixed           = #{const Z_FIXED}

withStreamPtr :: (Ptr StreamState -> IO a) -> Stream a
withStreamPtr f = do
  stream <- getStreamState
  unsafeLiftIO (withForeignPtr stream f)

withStreamState :: (StreamState -> IO a) -> Stream a
withStreamState f = do
  stream <- getStreamState
  unsafeLiftIO (withForeignPtr stream (f . StreamState))

setInAvail :: Int -> Stream ()
setInAvail val = withStreamPtr $ \ptr ->
  #{poke z_stream, avail_in} ptr (fromIntegral val :: CUInt)

getInAvail :: Stream Int
getInAvail = liftM (fromIntegral :: CUInt -> Int) $
  withStreamPtr (#{peek z_stream, avail_in})

setInNext :: Ptr Word8 -> Stream ()
setInNext val = withStreamPtr (\ptr -> #{poke z_stream, next_in} ptr val)

getInNext :: Stream (Ptr Word8)
getInNext = withStreamPtr (#{peek z_stream, next_in})

setOutFree :: Int -> Stream ()
setOutFree val = withStreamPtr $ \ptr ->
  #{poke z_stream, avail_out} ptr (fromIntegral val :: CUInt)

getOutFree :: Stream Int
getOutFree = liftM (fromIntegral :: CUInt -> Int) $
  withStreamPtr (#{peek z_stream, avail_out})

setOutNext  :: Ptr Word8 -> Stream ()
setOutNext val = withStreamPtr (\ptr -> #{poke z_stream, next_out} ptr val)

getOutNext :: Stream (Ptr Word8)
getOutNext = withStreamPtr (#{peek z_stream, next_out})

inflateInit :: Format -> WindowBits -> Stream ()
inflateInit format bits = do
  checkFormatSupported format
  err <- withStreamState $ \zstream ->
    c_inflateInit2 zstream (fromIntegral (windowBits format bits))
  failIfError err
  getStreamState >>= unsafeLiftIO . addForeignPtrFinalizer c_inflateEnd

deflateInit :: Format
            -> CompressionLevel
            -> Method
            -> WindowBits
            -> MemoryLevel
            -> CompressionStrategy
            -> Stream ()
deflateInit format compLevel method bits memLevel strategy = do
  checkFormatSupported format
  err <- withStreamState $ \zstream ->
    c_deflateInit2 zstream
                  (fromCompressionLevel compLevel)
                  (fromMethod method)
                  (windowBits format bits)
                  (fromMemoryLevel memLevel)
                  (fromCompressionStrategy strategy)
  failIfError err
  getStreamState >>= unsafeLiftIO . addForeignPtrFinalizer c_deflateEnd

inflate_ :: Flush -> Stream Status
inflate_ flush = do
  err <- withStreamState $ \zstream ->
    c_inflate zstream (fromFlush flush)
  failIfError err
  return (toStatus err)

deflate_ :: Flush -> Stream Status
deflate_ flush = do
  err <- withStreamState $ \zstream ->
    c_deflate zstream (fromFlush flush)
  failIfError err
  return (toStatus err)

-- | This never needs to be used as the stream's resources will be released
-- automatically when no longer needed, however this can be used to release
-- them early. Only use this when you can guarantee that the stream will no
-- longer be needed, for example if an error occurs or if the stream ends.
--
finalise :: Stream ()
finalise = getStreamState >>= unsafeLiftIO . finalizeForeignPtr

checkFormatSupported :: Format -> Stream ()
checkFormatSupported format = do
  version <- unsafeLiftIO (peekCAString =<< c_zlibVersion)
  case version of
    ('1':'.':'1':'.':_)
       | format == GZip
      || format == GZipOrZlib
      -> fail $ "version 1.1.x of the zlib C library does not support the"
             ++ " 'gzip' format via the in-memory api, only the 'raw' and "
	     ++ " 'zlib' formats."
    _ -> return ()

----------------------
-- The foreign imports

newtype StreamState = StreamState (Ptr StreamState)

-- inflateInit2 and deflateInit2 are actually defined as macros in zlib.h
-- They are defined in terms of inflateInit2_ and deflateInit2_ passing two
-- additional arguments used to detect compatability problems. They pass the
-- version of zlib as a char * and the size of the z_stream struct.
-- If we compile via C then we can avoid this hassle however thats not really
-- kosher since the Haskell FFI is defined at the C ABI level, not the C
-- language level. There is no requirement to compile via C and pick up C
-- headers. So it's much better if we can make it work properly and that'd
-- also allow compiling via ghc's ncg which is a good thing since the C
-- backend is not going to be around forever.
--
-- So we define c_inflateInit2 and c_deflateInit2 here as wrappers around
-- their _ counterparts and pass the extra args.

foreign import ccall unsafe "zlib.h inflateInit2_"
  c_inflateInit2_ :: StreamState -> CInt -> Ptr CChar -> CInt -> IO CInt

c_inflateInit2 :: StreamState -> CInt -> IO CInt
c_inflateInit2 z n =
  withCAString #{const_str ZLIB_VERSION} $ \versionStr ->
    c_inflateInit2_ z n versionStr (#{const sizeof(z_stream)} :: CInt)

foreign import ccall unsafe "zlib.h inflate"
  c_inflate :: StreamState -> CInt -> IO CInt

foreign import ccall unsafe "zlib.h &inflateEnd"
  c_inflateEnd :: FinalizerPtr StreamState


foreign import ccall unsafe "zlib.h deflateInit2_"
  c_deflateInit2_ :: StreamState
                  -> CInt -> CInt -> CInt -> CInt -> CInt
		  -> Ptr CChar -> CInt
		  -> IO CInt

c_deflateInit2 :: StreamState
               -> CInt -> CInt -> CInt -> CInt -> CInt -> IO CInt
c_deflateInit2 z a b c d e =
  withCAString #{const_str ZLIB_VERSION} $ \versionStr ->
    c_deflateInit2_ z a b c d e versionStr (#{const sizeof(z_stream)} :: CInt)

foreign import ccall unsafe "zlib.h deflate"
  c_deflate :: StreamState -> CInt -> IO CInt

foreign import ccall unsafe "zlib.h &deflateEnd"
  c_deflateEnd :: FinalizerPtr StreamState

foreign import ccall unsafe "zlib.h zlibVersion"
  c_zlibVersion :: IO CString
