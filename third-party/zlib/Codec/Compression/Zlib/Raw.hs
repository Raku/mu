-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) 2006 Duncan Coutts
-- License     :  BSD-style
--
-- Maintainer  :  duncan.coutts@worc.ox.ac.uk
-- Stability   :  experimental
-- Portability :  portable (H98 + FFI)
--
-- Compression and decompression of data streams in the raw deflate format.
--
-- The format is described in detail in RFC #1951:
-- <http://www.ietf.org/rfc/rfc1951.txt>
--
-- See also the zlib home page: <http://zlib.net/>
--
-----------------------------------------------------------------------------
module Codec.Compression.Zlib.Raw (
  
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
decompress = Internal.decompressDefault Raw

compress :: ByteString -> ByteString
compress = Internal.compressDefault Raw DefaultCompression

compressWith ::CompressionLevel -> ByteString -> ByteString
compressWith = Internal.compressDefault Raw
