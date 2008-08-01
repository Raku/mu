-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) 2006 Duncan Coutts
-- License     :  BSD-style
--
-- Maintainer  :  duncan.coutts@worc.ox.ac.uk
-- Stability   :  provisional
-- Portability :  portable (H98 + FFI)
--
-- Compression and decompression of data streams in the zlib format.
--
-- The format is described in detail in RFC #1950:
-- <http://www.ietf.org/rfc/rfc1950.txt>
--
-- See also the zlib home page: <http://zlib.net/>
--
-----------------------------------------------------------------------------
module Codec.Compression.Zlib (
  
  -- * Compression
  compress,
  compressWith,
  CompressionLevel(..),
  
  -- * Decompression
  decompress

  ) where

import Data.ByteString.Lazy (ByteString)

import Codec.Compression.Zlib.Internal as Internal

decompress :: ByteString -> ByteString
decompress = Internal.decompressDefault Zlib

compress :: ByteString -> ByteString
compress = Internal.compressDefault Zlib DefaultCompression

compressWith ::CompressionLevel -> ByteString -> ByteString
compressWith = Internal.compressDefault Zlib
