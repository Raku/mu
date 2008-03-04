{-# OPTIONS_GHC -fglasgow-exts -fno-warn-orphans -fno-full-laziness -fno-cse -fno-warn-deprecations -fallow-undecidable-instances -fallow-overlapping-instances -funbox-strict-fields -cpp #-}

module Pugs.Internals.ID (
    ID(..), bufToID, hashNew,
    __, (+++), nullID, 
) where

import System.IO.Unsafe
import Data.ByteString (ByteString)
import Pugs.Internals.Cast
import Data.Dynamic hiding (cast)
import Data.Generics (Data)
import qualified Data.HashTable as H
import qualified Foreign as Foreign
import qualified UTF8

{-
#if __GLASGOW_HASKELL__ > 606

-- If the following import directive triggers a compilation failure,
-- you need a newer GHC 6.7 snapshot.

import GHC.Base (IsString(..))

instance IsString ByteString where
    fromString = UTF8.pack
instance IsString ID where
    fromString = cast

#endif
-}

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
    readsPrec p s = [ (unsafePerformIO (bufToID (UTF8.pack x)), y) | (x, y) <- readsPrec p s]

{-# NOINLINE nullID #-}
nullID :: ID
nullID = _cast ""

{-# INLINE __ #-}
__ :: String -> ByteString
__ = UTF8.pack

{-# INLINE (+++) #-}
(+++) :: ByteString -> ByteString -> ByteString
(+++) = UTF8.append

{-# INLINE hashNew #-}
hashNew :: IO (H.HashTable ByteString a)
hashNew = H.new (==) (UTF8.hash)

{-# NOINLINE _BufToID #-}
_BufToID :: H.HashTable ByteString ID
_BufToID = unsafePerformIO hashNew

{-# NOINLINE _ID_count #-}
_ID_count :: Foreign.Ptr Int
_ID_count = unsafePerformIO (Foreign.new 1)

instance ((:>:) ID) String where
    cast str = let i = unsafePerformIO (bufToID (cast str)) in idKey `seq` i

instance ((:>:) String) ID where
    cast = cast . idBuf

instance ((:<:) String) ID where
    castBack = cast

instance ((:<:) ID) ByteString where
    castBack = idBuf

instance ((:<:) ByteString) ID where
    castBack buf = let i = unsafePerformIO (bufToID buf) in idKey i `seq` i

{-# NOINLINE bufToID #-}
bufToID :: ByteString -> IO ID
bufToID buf = do
    a'      <- H.lookup _BufToID buf
    case a' of
        Just a  -> do
            -- hPrint stderr ("HIT", buf, W# (unsafeCoerce# _BufToID))
            return a
        _       -> do
            i <- Foreign.peek _ID_count
            -- hPrint stderr ("MISS", buf, W# (unsafeCoerce# _BufToID), i)
            Foreign.poke _ID_count (succ i)
            let a = MkID{ idKey = i, idBuf = buf }
            H.insert _BufToID buf a
            return a

