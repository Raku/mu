{-# OPTIONS_GHC -cpp -fglasgow-exts #-}
--
-- Module      : Data.ByteString.UTF8
-- Copyright   : (c) Martin Norbäck 2006
-- License     : BSD-style
--
-- Maintainer  : martin@norpan.org
-- Stability   : experimental
-- Portability : same as Data.ByteString
-- 

--
-- | Manipulate ByteStrings containing UTF-8 encoded characters.
-- This is especially useful when doing mmapFile on a UTF-8 encoded file
-- because it will allow using the full Unicode range on Chars.
--
-- Behaviour when contents is not UTF-8 is undefined.
-- 
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with Prelude functions.  eg.
--
-- > import qualified Data.ByteString.Char8 as B
--

module UTF8 (

        -- * The @ByteString@ type
        ByteString(..),         -- instances: Eq, Ord, Show, Read, Data, Typeable

        -- * Introducing and eliminating 'ByteString's
        empty,                  -- :: ByteString
        packChar,               -- :: Char   -> ByteString
        pack,                   -- :: String -> ByteString
        unpack,                 -- :: ByteString -> String

        -- * Basic interface
        cons,                   -- :: Char -> ByteString -> ByteString
        snoc,                   -- :: ByteString -> Char -> ByteString
        null,                   -- :: ByteString -> Bool
        length,                 -- :: ByteString -> Int
        head,                   -- :: ByteString -> Char
        tail,                   -- :: ByteString -> ByteString
        last,                   -- :: ByteString -> Char
        init,                   -- :: ByteString -> ByteString
        append,                 -- :: ByteString -> ByteString -> ByteString

        -- * Reducing 'ByteString's
        foldl,                  -- :: (a -> Char -> a) -> a -> ByteString -> a
        foldr,                  -- :: (Char -> a -> a) -> a -> ByteString -> a
        foldl1,                 -- :: (Char -> Char -> Char) -> ByteString -> Char
        foldr1,                 -- :: (Char -> Char -> Char) -> ByteString -> Char

        -- ** Special folds
        concat,                 -- :: [ByteString] -> ByteString
        concatMap,              -- :: (Char -> ByteString) -> ByteString -> ByteString

        -- ** Joining strings
        join,                   -- :: ByteString -> [ByteString] -> ByteString

        -- ** Searching for substrings
        isPrefixOf,             -- :: ByteString -> ByteString -> Bool
        isSuffixOf,             -- :: ByteString -> ByteString -> Bool
        isSubstringOf,          -- :: ByteString -> ByteString -> Bool
        findSubstring,          -- :: ByteString -> ByteString -> Maybe Int
        findSubstrings,         -- :: ByteString -> ByteString -> [Int]

        -- ** Using ByteStrings as CStrings
        useAsCString,           -- :: ByteString -> (CString -> IO a) -> IO a
        useAsCStringLen,        -- :: ByteString -> (CString -> IO a) -> IO a
        unsafeUseAsCString,     -- :: ByteString -> (CString -> IO a) -> IO a
        unsafeUseAsCStringLen,  -- :: ByteString -> (CStringLen -> IO a) -> IO a

        -- ** Copying ByteStrings
        -- | These functions perform memcpy(3) operations
        copy,                   -- :: ByteString -> ByteString
        copyCString,            -- :: CString -> ByteString
        copyCStringLen,         -- :: CStringLen -> ByteString

        -- * I\/O with @ByteString@s

        -- ** Standard input and output

#if defined(__GLASGOW_HASKELL__)
        getLine,                -- :: IO ByteString
#endif
        getContents,            -- :: IO ByteString
        putStr,                 -- :: ByteString -> IO ()
        putStrLn,               -- :: ByteString -> IO ()

        -- ** Files
        readFile,               -- :: FilePath -> IO ByteString
--      mmapFile,               -- :: FilePath -> IO ByteString
        writeFile,              -- :: FilePath -> ByteString -> IO ()

        -- ** I\/O with Handles
#if defined(__GLASGOW_HASKELL__)
--      getArgs,                -- :: IO [ByteString]
        hGetLine,               -- :: Handle -> IO ByteString
        hGetNonBlocking,        -- :: Handle -> Int -> IO ByteString
#endif
        hGetContents,           -- :: Handle -> IO ByteString
        hGet,                   -- :: Handle -> Int -> IO ByteString
        hPut,                   -- :: Handle -> ByteString -> IO ()

#if defined(__GLASGOW_HASKELL__)
        -- * Low level construction
        -- | For constructors from foreign language types see /Data.ByteString/
        packAddress,            -- :: Addr# -> ByteString
        unsafePackAddress,      -- :: Int -> Addr# -> ByteString
#endif

        -- simple list-using functions
take, drop, unlines, group, reverse, inits, tails, sort, splitAt,
index,
map,
filter,
filterChar,
filterNotChar,
takeWhile,
dropWhile,
span,
spanEnd,
break,
lines,
lines',
unlines',
split,
words,
unwords,
words',
unwords',
groupBy,
intersperse,
any,
all,
maximum,
minimum,
replicate,
elem,
notElem,
find,
elemIndex,
elemIndexLast,
findIndex,
elemIndices,
findIndices,
lineIndices,
breakChar,
breakSpace,
spanChar,
breakFirst,
splitWith,
tokens,
dropSpace,
dropSpaceEnd,
joinWithChar,
zip,
zipWith,
count,
unzip,
transpose
    ) where

import qualified Prelude as P
import Prelude hiding           (reverse,head,tail,last,init,null
                                ,length,map,lines,foldl,foldr,unlines
                                ,concat,any,take,drop,splitAt,takeWhile
                                ,dropWhile,span,break,elem,filter,unwords
                                ,words,maximum,minimum,all,concatMap
                                ,foldl1,foldr1,readFile,writeFile,replicate
                                ,getContents,getLine,putStr,putStrLn
                                ,zip,zipWith,unzip,notElem)

import qualified Data.ByteString as B
-- These functions are unchanged from Data.ByteString
-- which means they work for (valid) UTF-8 byte strings too
import Data.ByteString (empty,null,append
                       ,concat,join

                       -- for valid UTF-8 these functions work on the byte
                       -- level
                       ,isPrefixOf,isSuffixOf,isSubstringOf

                       ,copy
                       ,getContents, putStr, putStrLn
                       ,readFile, {-mmapFile,-} writeFile
                       ,hGetContents, hGet, hPut
                       ,copy, copyCString, copyCStringLen
                       ,singleton
                       )

import Data.ByteString.Base
                       ( ByteString(..), unsafeUseAsCString, unsafeUseAsCStringLen, unsafeCreate, memcpy
                       )
import Data.ByteString.Char8
                       (getLine, hGetLine, hGetNonBlocking
                       ,packAddress, unsafePackAddress
                       ,useAsCStringLen, useAsCString
                       )

import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import Data.Bits
import Data.Word (Word8)
import Control.Monad (when)
import Control.Exception (assert)
import Data.Maybe (listToMaybe)
import GHC.Base (build, unsafeChr)
import GHC.Prim (realWorld#)
import GHC.IOBase

import qualified Data.List as List
import qualified Data.Char as Char

#define STRICT1(f) f a | a `seq` False = undefined
#define STRICT2(f) f a b | a `seq` b `seq` False = undefined
#define STRICT3(f) f a b c | a `seq` b `seq` c `seq` False = undefined
#define STRICT4(f) f a b c d | a `seq` b `seq` c `seq` d `seq` False = undefined
#define STRICT5(f) f a b c d e | a `seq` b `seq` c `seq` d `seq` e `seq` False = undefined

newline, space :: Word8
newline = 0x0A
space   = 0x20

packChar :: Char -> ByteString
packChar c = pack [c]

pack :: String -> ByteString
pack s = unsafeCreate (numBytesString s) $ \p -> go p s
  where
    go _ [] = return ()
    go p (x:xs) = do
      l <- putUTF8 p x 
      go (p `plusPtr` l) xs

{-# INLINE unpack #-}
unpack :: ByteString -> String
unpack ps = build (unpackFoldr ps)

{-# INLINE [0] unpackFoldr #-}
unpackFoldr :: ByteString -> (Char -> a -> a) -> a -> a
unpackFoldr (PS x s l) f c = withPtr x $ \p ->
    go (p `plusPtr` s) l
  where
    STRICT2(go)
    go q n | n <= 0 = touchForeignPtr x >> return c
           | otherwise = do
      (e, w) <- getUTF8 q n
      es <- unsafeInterleaveIO $ go (q `plusPtr` w) (n-w)
      return (e `f` es)

{-# RULES
"unpack-list"  [1]  forall p  . unpackFoldr p (:) [] = unpackList p
 #-}

unpackList :: ByteString -> String
unpackList (PS x s l) = withPtr x $ \p ->
    go (p `plusPtr` s) l
  where
    STRICT2(go)
    go q n | n <= 0 = touchForeignPtr x >> return []
           | otherwise = do
      (e, w) <- getUTF8 q n
      es <- unsafeInterleaveIO $ go (q `plusPtr` w) (n-w)
      return (e : es)

{-
-- -----------------------------------------------------------------------------
-- unpacking

{-# INLINE unpack #-}
unpack :: ByteString -> [Char]
unpack ps = build (unpackFoldr ps)

{-# INLINE [0] unpackFoldr #-}
unpackFoldr (PS x s l) f c = withPtr x $ \p ->
      unpackFoldrCStringUtf8 (p `plusPtr` s) l f c

unpackFoldrCStringUtf8 :: Ptr Word8 -> Int -> (Char -> a -> a) -> a -> IO a
STRICT4(unpackFoldrCStringUtf8)
unpackFoldrCStringUtf8 addr len f c
  = unpack 0 c
  where
    unpack nh acc | nh >= len = return acc
    unpack nh acc = do
      (c,w) <- getUTF8 (addr `plusPtr` nh) (len - nh)
      unpack (nh+w) (c `f` acc)

unpackList (PS x s l) = withPtr x $ \p ->
      unpackCStringUtf8 (p `plusPtr` s) l

unpackCStringUtf8 :: Ptr Word8 -> Int -> IO [Char]
STRICT2(unpackCStringUtf8)
unpackCStringUtf8 addr len
  = unpack 0 []
  where
    unpack nh acc | nh >= len = return acc
    unpack nh acc = do
      (c,w) <- getUTF8 (addr `plusPtr` nh) (len - nh)
      unpack (nh+w) (c:acc)

{-# RULES
"unpack-list"  [1]  forall l   . unpackFoldr l (:) [] = unpackList l
 #-}
-}

cons :: Char -> ByteString -> ByteString
cons c (PS x s l) = let w = numBytes c in
  unsafeCreate (l + w) $ \p -> withForeignPtr x $ \f -> do
    memcpy (p `plusPtr` w) (f `plusPtr` s) (fromIntegral l)
    n <- putUTF8 p c
    assert (w == n) $ return ()

snoc :: ByteString -> Char -> ByteString
snoc (PS x s l) c = let w = numBytes c in
  unsafeCreate (l + w) $ \p -> withForeignPtr x $ \f -> do
    memcpy p (f `plusPtr` s) (fromIntegral l)
    n <- putUTF8 (p `plusPtr` l) c
    assert (w == n) $ return ()

-- length taks O(n) now because we need to traverse
-- find length by using full string length and subtract all "follow" bytes
length :: ByteString -> Int
length bs = B.length bs - P.length (B.findIndices isFollow bs)

head :: ByteString -> Char
head = fst . headTail

tail :: ByteString -> ByteString
tail = snd . headTail

headTail :: ByteString -> (Char, ByteString)
headTail (PS x s l) | l <= 0 = errorEmptyList "headTail"
                    | otherwise = withPtr x $ \p -> do
                                    (c,l') <- getUTF8 (p `plusPtr` s) l
                                    return (c, PS x (s+l') (l-l'))

last :: ByteString -> Char
last (PS x s l) | l <= 0 = errorEmptyList "last"
                | otherwise = withPtr x $ \p -> do
                                n <- backFollow (p `plusPtr` (s+l-1)) 0
                                (c,_) <- getUTF8 (p `plusPtr` (s+l-n-1)) (n+1)
                                return c

init :: ByteString -> ByteString
init (PS x s l) | l <= 0 = errorEmptyList "init"
                | otherwise = withPtr x $ \p -> do
                                n <- backFollow (p `plusPtr` (s+l-1)) 0
                                return (PS x s (l-n-1))

foldl :: (a -> Char -> a) -> a -> ByteString -> a
foldl f e ps | null ps = e
             | otherwise = let (h,t) = headTail ps in foldl f (f e h) t

foldl1 :: (Char -> Char -> Char) -> ByteString -> Char
foldl1 f ps | null ps = errorEmptyList "foldl1"
            | otherwise = let (h,t) = headTail ps in foldl f h t

foldr :: (Char -> a -> a) -> a -> ByteString -> a
foldr f e ps | null ps = e
             | otherwise = let (h,t) = headTail ps in f h (foldr f e t)

foldr1 :: (Char -> Char -> Char) -> ByteString -> Char
foldr1 f ps | null ps = errorEmptyList "foldr1"
            | otherwise = foldr f (last ps) (init ps)

concatMap :: (Char -> ByteString) -> ByteString -> ByteString
concatMap f = foldr (append . f) empty

-- return how many 10xxxxxx bytes there are backwards from the ptr
backFollow :: Ptr Word8 -> Int -> IO Int
backFollow p x = do
  when (x > 3) (fail "too many follow bytes")
  (c :: Word8) <- peekByteOff p (-x)
  if (not (isFollow c)) then
    return x
   else
    backFollow p (x+1)

-- put char into memory area and return number of bytes written
putUTF8 :: Ptr Word8 -> Char -> IO Int
putUTF8 p0 char =
  if ch < 0x80 then do
    poke p0 (toEnum ch)
    return 1
  else if ch < 0x800 then do
    poke p0 (toEnum (b11000000 .|. bits12))
    poke p1 (toEnum (b10000000 .|. bits6))
    return 2
  else if ch < 0x10000 then do
    poke p0 (toEnum (b11100000 .|. bits18))
    poke p1 (toEnum (b10000000 .|. bits12))
    poke p2 (toEnum (b10000000 .|. bits6))
    return 3
  else do -- if ch < 0x110000
    poke p0 (toEnum (b11110000 .|. bits24))
    poke p1 (toEnum (b10000000 .|. bits18))
    poke p2 (toEnum (b10000000 .|. bits12))
    poke p3 (toEnum (b10000000 .|. bits6))
    return 4
  where 
    -- all calculations are made in Int here, for speed and no risk of
    -- overflow, portability etc.
    ch     = fromEnum char
    bits6  = ch               .&. b00111111
    bits12 = (ch `shiftR` 6)  .&. b00111111
    bits18 = (ch `shiftR` 12) .&. b00111111
    bits24 = (ch `shiftR` 18) -- no mask, since Char is limited
    (p1 :: Ptr Word8) = p0 `plusPtr` 1
    (p2 :: Ptr Word8) = p0 `plusPtr` 2
    (p3 :: Ptr Word8) = p0 `plusPtr` 3

-- utf8 diagram
--          UTF-8                                Char
-- 1 byte   0xxxxxxx                             0xxxxxxx
-- 2 bytes  110zzzzx 10xxxxxx                    00000zzz zxxxxxxx
-- 3 bytes  1110zzzz 10zxxxxx 10xxxxxx           zzzzzxxx xxxxxxxx
-- 4 bytes  11110zzz 10zzxxxx 10xxxxxx 10xxxxxx  000zzzzz xxxxxxxx xxxxxxxx
-- the digits marked with z may not all be zero (overlong)
-- get one char from pointer location
-- only allowed to read int number of words
-- int must be at least 1
-- return bytes consumed
getUTF8 :: Ptr Word8 -> Int -> IO (Char, Int)
getUTF8 p m = do
  c <- fmap fromEnum (peek p)
  if c < b10000000 then         -- 0xxxxxxx
    return (unsafeChr c, 1)
   else if c < b11000000 then    -- 10xxxxxx
    fail "invalid first byte"
   else if c < b11100000 then do -- 110zzzzx
    r <- decodeFollow p 1 (c .&. b00011111) m
    when (r < 0x80) (fail "overlong")
    return (unsafeChr r, 2)
   else if c < b11110000 then do -- 1110xxxx
    r <- decodeFollow p 2 (c .&. b00001111) m
    when (r < 0x800) (fail "overlong")
    return (unsafeChr r, 3)
   else if c < b11111000 then do -- 11110xxx
    r <- decodeFollow p 3 (c .&. b00000111) m
    when (r < 0x10000) (fail "overlong")
    return (unsafeChr r, 4)
   else                     -- 11111xxx
    fail "unicode value too large"
 where
  decodeFollow :: (Ptr Word8) -> Int -> Int -> Int -> IO Int
  decodeFollow _ 0 c _ = return c
  decodeFollow _ _ _ 0 = fail "premature end of string"
  decodeFollow q n c o = do
    let q' = q `plusPtr` 1
    b <- fmap fromEnum (peek q' :: IO Word8)
    when (not (isFollow b)) (fail "invalid follow byte")
    decodeFollow q' (n-1) ((c `shiftL` 6) .|. (b .&. b00111111)) (o-1)

isFollow :: Bits a => a -> Bool
isFollow b = (b .&. b11000000) == b10000000
    
numBytes :: Char -> Int
numBytes c | fromEnum c < 0x80    = 1
           | fromEnum c < 0x800   = 2
           | fromEnum c < 0x10000 = 3
           | otherwise   = 4

numBytesString :: String -> Int
numBytesString s = sum (P.map numBytes s)

b00000111,b00001111,b00011111,b00111111,b11111000,b11110000,b11100000,b11000000,b10000000 :: Num a => a
b00000111 = 0x07
b00001111 = 0x0f
b00011111 = 0x1f
b00111111 = 0x3f
b11111000 = 0xf8
b11110000 = 0xf0
b11100000 = 0xe0
b11000000 = 0xc0
b10000000 = 0x80

-- Common up near identical calls to `error' to reduce the number
-- constant strings created when compiled:
errorEmptyList :: String -> a
errorEmptyList fun = error ("Data.ByteString.UTF8." ++ fun ++ ": empty ByteString")
{-# INLINE errorEmptyList #-}

-- unlines
unlines :: [ByteString] -> ByteString
unlines [] = empty
unlines ss = (concat $ List.intersperse nl ss) `append` nl -- half as much space
    where nl = singleton newline

-- unlines'
unlines' :: [ByteString] -> ByteString
unlines' [] = empty
unlines' ss = (concat $ List.intersperse nl ss)
    where nl = singleton newline

-- Below are simple, via-list implementations
-- ...should have great potential for optimization :-)

viaList :: ([Char] -> [Char]) -> ByteString -> ByteString
viaList f = pack . f . unpack

viaList2 :: ([Char]->([Char],[Char])) -> ByteString -> (ByteString,ByteString)
viaList2 f bs = let (a,b) = f (unpack bs) in (pack a, pack b)

take :: Int -> ByteString -> ByteString
take n = fst . splitAt n

drop :: Int -> ByteString -> ByteString
drop n = snd . splitAt n

inits :: ByteString -> [ByteString]
inits bs@(PS x s l) = [PS x s n | n <- rawIndices bs]

tails :: ByteString -> [ByteString]
tails bs = if null bs then [empty] else bs : tails (tail bs)

sort :: ByteString -> ByteString
sort = viaList List.sort

-- can be optimized to ignore follow bytes
splitAt :: Int -> ByteString -> (ByteString, ByteString)
splitAt n ps@(PS x s l) | n <= 0 = (empty, ps)
                        | otherwise = withPtr x $ \p -> go (p `plusPtr` s) 0 l
  where
    go q i m | m <= 0 = return (ps, empty)
             | i >= n = return (PS x s (l-m), PS x (s+l-m) m)
             | otherwise = do
      k <- rawLength q
      go (q `plusPtr` k) (i+1) (m-k)

group :: ByteString -> [ByteString]
group = groupBy (==)

reverse :: ByteString -> ByteString
reverse = pack . List.reverse . unpack

-- index
index :: ByteString -> Int -> Char
index = (!!) . unpack

-- map
map :: (Char -> Char) -> ByteString -> ByteString
map f = viaList (List.map f)

-- filter
filter :: (Char -> Bool) -> ByteString -> ByteString
filter p = viaList (List.filter p)

-- filterChar
filterChar :: Char -> ByteString -> ByteString
filterChar c = viaList (List.filter (==c))

-- filterNotChar
filterNotChar :: Char -> ByteString -> ByteString
filterNotChar c = viaList (List.filter (/=c))

-- takeWhile
takeWhile :: (Char -> Bool) -> ByteString -> ByteString
takeWhile p = fst . span p

-- dropWhile
dropWhile :: (Char -> Bool) -> ByteString -> ByteString
dropWhile p = snd . span p

-- span
span :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
span p = break (not . p)

-- spanEnd
spanEnd :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
spanEnd p = error "not implemented yet"

-- lines
-- TODO: implement other types of line breaks!
lines :: ByteString -> [ByteString]
lines ps
    | null ps = []
    | otherwise = case search ps of
             Nothing -> [ps]
             Just n  -> B.take n ps : lines (B.drop (n+1) ps)
    where search = B.elemIndex newline

lines' :: ByteString -> [ByteString]
lines' bs = lines bs ++ 
  if (not (null bs) && B.last bs == newline) then [singleton newline] else []

-- split
split :: Char -> ByteString -> [ByteString]
split c = let i = Char.ord c in 
          if i < 128 then B.split (fromIntegral i)
          else splitWith (== c)

-- words
words :: ByteString -> [ByteString]
words = tokens Char.isSpace

-- unwords
unwords :: [ByteString] -> ByteString
unwords = join (singleton space)

words' :: ByteString -> [ByteString]
words' = splitWith Char.isSpace

unwords' :: [ByteString] -> ByteString
unwords' = unwords

-- groupBy
groupBy :: (Char -> Char -> Bool) -> ByteString -> [ByteString]
groupBy f (PS x s l) = withPtr x $ \p -> go (p `plusPtr` s) 0 l
 where
  STRICT3(go)
  go q r m | m <= 0 = touchForeignPtr x >> return []
           | otherwise = do
    (c,w) <- getUTF8 q m
    mi <- findRawIndex (not . f c) (q `plusPtr` w) (m-w)
    case mi of
      Nothing -> return [PS x (s+r) m]
      Just i -> do
        rest <- unsafeInterleaveIO $ go (q `plusPtr` (w+i)) (r+w+i) (m-w-i)
        return (PS x (s+r) (i+w):rest)

-- intersperse
intersperse :: Char -> ByteString -> ByteString
intersperse c = viaList (List.intersperse c)

-- any
any :: (Char -> Bool) -> ByteString -> Bool
any f (PS x s l) = withPtr x $ \p -> do
  mi <- findRawIndex f (p `plusPtr` s) l
  return $ case mi of
    Nothing -> False
    Just _ -> True

-- all
all :: (Char -> Bool) -> ByteString -> Bool
all p = not . any (not . p)

-- maximum
maximum :: ByteString -> Char
maximum = foldl1 max

-- minimum
minimum :: ByteString -> Char
minimum = foldl1 min

-- replicate
replicate :: Int -> Char -> ByteString
replicate n c | n <= 0 = empty
              | Char.ord c < 128 = B.replicate n (fromIntegral $ Char.ord c)
              | otherwise = unsafeCreate (n * numBytes c) $ \p -> go n p
  where
    go 0 p = return ()
    go n p = do
      k <- putUTF8 p c 
      go (n-1) (p `plusPtr` k)

-- elem
elem :: Char -> ByteString -> Bool
elem c = if Char.ord c < 128 then B.elem (fromIntegral $ Char.ord c)
         else List.elem c . unpack

-- notElem
notElem :: Char -> ByteString -> Bool
notElem c = not . elem c

-- find
find :: (Char -> Bool) -> ByteString -> Maybe Char
find p = List.find p . unpack

-- elemIndex
elemIndex :: Char -> ByteString -> Maybe Int
elemIndex c = List.elemIndex c . unpack

-- elemIndexLast
elemIndexLast :: Char -> ByteString -> Maybe Int
elemIndexLast c = undefined

-- findIndex
findIndex :: (Char -> Bool) -> ByteString -> Maybe Int
findIndex p = List.findIndex p . unpack

-- elemIndices
elemIndices :: Char -> ByteString -> [Int]
elemIndices c = List.elemIndices c . unpack

-- findIndices
findIndices :: (Char -> Bool) -> ByteString -> [Int]
findIndices p = List.findIndices p . unpack

-- lineIndices
lineIndices :: ByteString -> [Int]
lineIndices = elemIndices '\n'

-- breakChar
breakChar :: Char -> ByteString -> (ByteString, ByteString)
breakChar c = viaList2 (List.break (==c))

-- breakSpace
breakSpace :: ByteString -> (ByteString,ByteString)
breakSpace = break Char.isSpace

-- spanChar
spanChar :: Char -> ByteString -> (ByteString, ByteString)
spanChar c = viaList2 (List.span (==c))

-- splitWith
splitWith :: (Char -> Bool) -> ByteString -> [ByteString]
splitWith f ps | null ps = []
               | otherwise =
  let (first, rest) = break f ps
      t = tail rest
  -- need to add extra empty string if char splitted on was the last one
  in first:if null rest then [] else if null t then [empty] else splitWith f t

-- tokens
tokens :: (Char -> Bool) -> ByteString -> [ByteString]
tokens f = P.filter (not.null) . splitWith f

-- dropSpace
dropSpace :: ByteString -> ByteString
dropSpace = dropWhile Char.isSpace

-- dropSpaceEnd
dropSpaceEnd :: ByteString -> ByteString
dropSpaceEnd = undefined

-- joinWithChar
joinWithChar :: Char -> ByteString -> ByteString -> ByteString
joinWithChar c b1 b2 = concat [b1, packChar c, b2]

-- zip
zip :: ByteString -> ByteString -> [(Char,Char)]
zip = zipWith (,)

-- zipWith
zipWith :: (Char -> Char -> a) -> ByteString -> ByteString -> [a]
zipWith f b1 b2 | null b1 || null b2 = []
                | otherwise = f (head b1) (head b2) 
                            : zipWith f (tail b1) (tail b2)

-- count
count :: Char -> ByteString -> Int
count c bs | Char.ord c < 128 = B.count (fromIntegral (Char.ord c)) bs
           | otherwise = List.length (findSubstrings (packChar c) bs)

-- | Perform an operation with a temporary ByteString
withPtr :: ForeignPtr a -> (Ptr a -> IO b) -> b
withPtr fp io = inlinePerformIO (withForeignPtr fp io)
{-# INLINE withPtr #-}

unzip :: [(Char,Char)] -> (ByteString,ByteString)
unzip ls = (pack (P.map fst ls), pack (P.map snd ls))

transpose :: [ByteString] -> [ByteString]
transpose ps = P.map pack (List.transpose (P.map unpack ps))

break :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
break f ps@(PS x s l) = withPtr x $ \p -> do
  mi <- findRawIndex f (p `plusPtr` s) l
  return $ case mi of
    Nothing -> (ps, empty)
    Just i -> (PS x s i, PS x (s+i) (l-i))

-- internal function
-- find raw index in byte array for first char matching
findRawIndex :: (Char -> Bool) -> Ptr Word8 -> Int -> IO (Maybe Int)
findRawIndex f p l = go p 0
  where
    STRICT2(go)
    go q i | i >= l = return Nothing
           | otherwise = do
      (e, w) <- getUTF8 q (l-i)
      if f e then
        return (Just i)
       else
        go (q `plusPtr` w) (i+w)

-- assumes correct input
rawLength :: Ptr Word8 -> IO Int
rawLength p = do
  c <- fmap fromEnum (peek p)
  return $
    if c < b10000000 then 1
    else if c < b11100000 then 2
    else if c < b11110000 then 3
    else if c < b11111000 then 4
    else 5 -- any number would do

rawIndices:: ByteString -> [Int]
rawIndices (PS x s l) = withPtr x $ \p -> go 0 (p `plusPtr` s) l
  where
    STRICT3(go)
    go k q m | m <= 0 = touchForeignPtr x >> return [k]
             | otherwise = do
      i <- rawLength q
      is <- unsafeInterleaveIO $ go (k+i) (q `plusPtr` i) (m-i)
      return (k:is)
  
breakFirst c xs = let (x,y) = breakChar c xs in 
  if null y then Nothing else Just (x, tail y)

findSubstring :: ByteString -> ByteString -> Maybe Int
findSubstring b1 b2 = listToMaybe (findSubstrings b1 b2)

-- use regular findSubstrings and map results back
findSubstrings :: ByteString -> ByteString -> [Int]
findSubstrings b1 b2@(PS x s l) | null b1 = [0 .. length b2]
                                | otherwise =
  [ i | (i,b) <- P.zip [0..] (tails b2), b1 `isPrefixOf` b ]


-- Just like inlinePerformIO, but we inline it. Big performance gains as
-- it exposes lots of things to further inlining
--
{-# INLINE inlinePerformIO #-}
inlinePerformIO :: IO a -> a
#if defined(__GLASGOW_HASKELL__)
inlinePerformIO (IO m) = case m realWorld# of (# _, r #) -> r
#else
inlinePerformIO = unsafePerformIO
#endif
