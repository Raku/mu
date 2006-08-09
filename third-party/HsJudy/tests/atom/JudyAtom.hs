module JudyAtom where

-- This code is based on Atom.hs from JHC

import qualified Judy.StrMap as M
import qualified Judy.IntMap as L
import qualified Judy.CollectionsM as C

import Judy.Refeable as R

import qualified Data.ByteString.Char8 as B

import Foreign
import Data.Typeable
import System.IO.Unsafe

instance R.Refeable Atom where
    toRef (Atom i) = return $ toEnum i
    fromRef v = return $ Atom (fromEnum v)
    needGC _ = False

{-# NOINLINE table #-}
table :: M.StrMap B.ByteString Atom
table = unsafePerformIO $ C.new

{-# NOINLINE reverseTable #-}
reverseTable :: L.IntMap Int B.ByteString
reverseTable = unsafePerformIO $ C.new

{-# NOINLINE intPtr #-}
intPtr :: Ptr Int
intPtr = unsafePerformIO (new 1)


newtype Atom = Atom Int
    deriving (Typeable, Eq, Ord)

instance Show Atom where
    showsPrec _ atom = (toStr atom ++)

instance Read Atom where
    readsPrec _ s = [ (fromStr s,"") ]
    --readsPrec p s = [ (fromStr x,y) |  (x,y) <- readsPrec p s]


class ToAtom a where
    toAtom :: a -> Atom
class FromAtom a where
    fromAtom :: Atom -> a


instance ToAtom String where
    toAtom = fromStr
instance FromAtom String where
    fromAtom = toStr
--instance FromAtom (String -> String) where
--    fromAtom x = showsPS (fromAtom x)


instance ToAtom B.ByteString where
    toAtom x = unsafePerformIO $ fromByteStringIO x
instance FromAtom B.ByteString where
    fromAtom = toByteString

instance ToAtom Atom where
    toAtom x = x
instance FromAtom Atom where
    fromAtom x = x




toStr a = B.unpack $ toByteString a
toByteString atom = atomToBS atom
atomIndex (Atom x) = x

fromStr :: String -> Atom
fromStr xs = unsafePerformIO $ fromStrIO xs

fromStrIO :: String -> IO Atom
fromStrIO cs = fromByteStringIO (B.pack cs)

fromByteStringIO :: B.ByteString -> IO Atom
fromByteStringIO bs = do
    a' <- C.lookup bs table
    case a' of
        Just a -> return a
        Nothing -> do
            i <- peek intPtr
            poke intPtr (i + 2)
            let a = Atom i
            C.insert bs a table
            C.insert i bs reverseTable
            return a

intToAtom :: Monad m => Int -> m Atom
intToAtom i = unsafePerformIO $ do
    bs' <- C.lookup i reverseTable
    case bs' of
        Just _ -> return (return $ Atom i)
        Nothing -> return $ fail $ "intToAtom: " ++ show i

atomToBS :: Atom -> B.ByteString
atomToBS !(Atom i) = unsafePerformIO $ do
    bs' <- C.lookup i reverseTable
    case bs' of
        Just bs -> return bs
        Nothing -> return $ error $ "atomToBS: " ++ show i
