{-# OPTIONS_GHC -fglasgow-exts #-}
{-# OPTIONS_GHC -#include "UnicodeC.h" #-}

module PIL.Str where
import qualified UTF8
import qualified Data.PackedString as PS
import Foreign.C.String
import PIL.Internals
import System.IO

type Str = PS.PackedString

fromString :: String -> Str
fromString = PS.packString

toString :: Str -> String
toString = PS.unpackPS

from :: (Show a) => a -> Str
from = PS.packString . Prelude.show

put :: Handle -> Str -> IO ()
put fh = hPutStr fh . toString
