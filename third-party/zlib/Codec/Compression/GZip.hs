-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) 2006 Duncan Coutts
-- License     :  BSD-style
--
-- Maintainer  :  duncan.coutts@worc.ox.ac.uk
-- Stability   :  provisional
-- Portability :  portable (H98 + FFI)
--
-- Compression and decompression of data streams in the gzip format.
--
-- The format is described in detail in RFC #1952:
-- <http://www.ietf.org/rfc/rfc1952.txt>
--
-- See also the zlib home page: <http://zlib.net/>
--
-----------------------------------------------------------------------------
module Codec.Compression.GZip (

  -- | This module provides pure functions for compressing and decompressing
  -- streams of data represented by lazy 'ByteString's. This makes it easy to
  -- use either in memory or with disk or network IO.
  --
  -- For example a simple gzip compression program is just:
  --
  -- > import qualified Data.ByteString.Lazy as ByteString
  -- > import qualified Codec.Compression.GZip as GZip
  -- >
  -- > main = ByteString.interact GZip.compress
  --
  -- Or you could lazily read in and decompress a @.gz@ file using:
  --
  -- > content <- fmap GZip.decompress (readFile file)
  --

  -- * Compression
  compress,
  compressWith,
  CompressionLevel(..),

  -- * Decompression
  decompress

  ) where

import Data.ByteString.Lazy (ByteString)

import Codec.Compression.Zlib.Internal as Internal


-- | Decompress a stream of data in the gzip format.
--
-- There are a number of errors that can occur. In each case an exception will
-- be thrown. The possible error conditions are:
--
-- * if the stream does not start with a valid gzip header
--
-- * if the compressed stream is corrupted
--
-- * if the compressed stream ends permaturely
--
-- Note that the decompression is performed /lazily/. Errors in the data stream
-- may not be detected until the end of the stream is demanded (since it is
-- only at the end that the final checksum can be checked). If this is
-- important to you, you must make sure to consume the whole decompressed
-- stream before doing any IO action that depends on it.
--
decompress :: ByteString -> ByteString
decompress = Internal.decompressDefault GZip


-- | Compress a stream of data into the gzip format.
--
-- This uses the default compression level which favours a higher compression
-- ratio over compression speed. Use 'compressWith' to adjust the compression
-- level.
--
compress :: ByteString -> ByteString
compress = Internal.compressDefault GZip DefaultCompression


-- | Like 'compress' but with an extra parameter to specify the compression
-- level.
--
-- There are a number of additional compression parameters which are rarely
-- necessary to change but if you need to you can do so using 'compressFull'.
--
compressWith ::CompressionLevel -> ByteString -> ByteString
compressWith = Internal.compressDefault GZip
