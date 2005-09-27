{-# OPTIONS_GHC -fglasgow-exts #-}
{-# OPTIONS_GHC -#include "UnicodeC.h" #-}

module PIL.Str where
import qualified Data.FastPackedString as P
import Foreign.C.String
import PIL.Internals
import System.IO

type Str = P.FastString

fromString :: String -> Str
fromString = P.pack

toString :: Str -> String
toString = P.unpack

from :: (Show a) => a -> Str
from = P.pack . Prelude.show

put :: Handle -> Str -> IO ()
put = P.hPut
