{-# OPTIONS_GHC -fglasgow-exts -fvia-C #-}

{-# INCLUDE <pcre.h> #-}
-- \#include <sys/types.h>
-- arch-tag: 0852a460-683f-4abb-9108-8205777e2033

module RRegex.PCRE (
    Regex,	 	-- abstract
    compile,
    execute,
    executeExtract,
    pcreCaseless,   --  case insensitive matching
    pcreMultiline,  --  ^ and $ match newline as well as beginning and end of string
    pcreDotall,     --  dot matches everything. including newline
    pcreExtended,  
    pcreAnchored, 
    pcreDollarEndonly, 
    pcreExtra, 
    pcreNotbol, 
    pcreNoteol, 
    pcreUngreedy,   --  matches are not greedy by default
    pcreNotempty,   --  refuse to match empty string
    pcreUtf8,       --  UTF-8 semantics
    
    getVersion
    ) where


import Prelude

import Foreign
import Foreign.C
import Foreign.C.String
import Array

-- | return version of pcre used or Nothing if pcre is not available.
getVersion :: Maybe String

data PCRE 

-- | A compiled regular expression
newtype Regex = Regex (ForeignPtr PCRE)
    deriving (Show, Eq, Ord)

fi :: (Num b, Integral a) => a -> b
fi x = fromIntegral x

-- | Compiles a regular expression
compile
  :: String  	-- ^ The regular expression to compile
  -> Int    	-- ^ Flags (summed together)
  -> IO (Either (Int,String) Regex)      -- ^ Returns: an error string and offset or the compiled regular expression
compile pattern flags = withCString pattern $ \cstr -> 
    alloca $ \errOffset -> alloca $ \errPtr -> do
        v <- c_pcre_compile cstr (fromIntegral flags) errPtr errOffset nullPtr
        if v == nullPtr then do
            es <- peek errPtr >>= peekCString
            eo <- peek errOffset
            return $ Left (fi eo,es)
          else fmap (Right . Regex) (newForeignPtr_ v)

getNumSubs :: Ptr PCRE -> IO CInt
getNumSubs (pcre_ptr) = 
    --withForeignPtr pcre_fptr $ \pcre_ptr -> 
        alloca $ \st -> do
            c_pcre_fullinfo pcre_ptr nullPtr (fi pcreInfoCapturecount) (st :: Ptr CInt)
            peek st

-- | Matches a regular expression against a string
execute :: Regex			-- ^ Compiled regular expression
	-> String			-- ^ String to match against
        -> Int                          -- ^ Options
	-> IO (Maybe (Array Int (Int,Int)))
	 	-- ^ Returns: 'Nothing' if the regex did not match the
		-- string, or:
		--   'Just' an array of (offset,length) pairs where index 0 is whole match, and the rest are the captured subexpressions.

execute (Regex pcre_fptr) str _ = withCStringLen str $ \(cstr,clen) -> 
    withForeignPtr pcre_fptr $ \pcre_ptr -> do
      nsub <- getNumSubs pcre_ptr 
      let nsub_int = fromIntegral ((nsub + 1) * 3)
      allocaBytes (nsub_int * (4)) $ \p_match -> do
        r <- c_pcre_exec pcre_ptr nullPtr cstr (fi clen) 0 0 p_match (fi nsub_int) 
        if r < 0 then return Nothing else do
            ri <- mapM (peekElemOff p_match) [0 .. fi r*2 - 1]
            let f [] = []
                f (a:b:rest) = (fromIntegral a,fromIntegral (b - a)):f rest
                f _ = error "unmatched pair"
            let ex = fi $ nsub + 1 - r
            return $ Just (listArray (0,fi nsub) ((f ri) ++ replicate ex (-1,0))) 
            
-- | execute match and extract substrings rather than just offsets
executeExtract  :: Regex     -- ^ compiled regular expression
                -> String    -- ^ string to match
                -> Int       -- ^ Flags (summed together)
                -> IO (Maybe (String, String, (Array Int String))) 
                      -- ^ Returns: Nothing if no match, else 
                      --   (text before match, text after match, array of matches with 0 being the whole match) 
executeExtract pcre str flags = do
    a <- execute pcre str flags
    case a of 
        Nothing -> return Nothing
        Just a -> return $ Just (before,after,fmap f a) where
            (bo, bl) = a ! 0
            before = take bo str
            after = drop (bo + bl) str
            f (o,l) = take l (drop o str)

getVersion = unsafePerformIO $ do
    s <- c_pcre_version 
    hs <- peekCString s
    return $ Just hs


foreign import ccall unsafe "pcre.h pcre_compile" 
    c_pcre_compile :: Ptr CChar -> CInt -> Ptr (Ptr CChar) -> Ptr CInt -> Ptr CChar -> IO (Ptr PCRE)
foreign import ccall unsafe "pcre.h pcre_exec" 
    c_pcre_exec :: Ptr PCRE -> Ptr () -> Ptr CChar -> CInt -> CInt -> CInt -> Ptr CInt -> CInt -> IO CInt
foreign import ccall unsafe "pcre.h pcre_fullinfo"
    c_pcre_fullinfo :: Ptr PCRE -> Ptr () -> CInt -> Ptr a -> IO CInt
foreign import ccall unsafe "pcre.h pcre_version"
    c_pcre_version :: IO (Ptr CChar)
-- foreign import ccall unsafe "pcre.h &pcre_free"
--    c_ptr_free :: FunPtr (Ptr a -> IO ())

    

pcreCaseless :: Int
pcreCaseless =  1
pcreMultiline :: Int
pcreMultiline =  2
pcreDotall :: Int
pcreDotall =  4
pcreExtended :: Int
pcreExtended =  8
pcreAnchored :: Int
pcreAnchored =  16
pcreDollarEndonly :: Int
pcreDollarEndonly =  32
pcreExtra :: Int
pcreExtra =  64
pcreNotbol :: Int
pcreNotbol =  128
pcreNoteol :: Int
pcreNoteol =  256
pcreUngreedy :: Int
pcreUngreedy =  512
pcreNotempty :: Int
pcreNotempty =  1024
pcreUtf8  :: Int
pcreUtf8  =  2048

{-
pcreErrorNomatch :: Int
pcreErrorNomatch =  (-1)
pcreErrorNull :: Int
pcreErrorNull =  (-2)
pcreErrorBadoption :: Int
pcreErrorBadoption =  (-3)
pcreErrorBadmagic :: Int
pcreErrorBadmagic =  (-4)
pcreErrorUnknownNode :: Int
pcreErrorUnknownNode =  (-5)
pcreErrorNomemory :: Int
pcreErrorNomemory =  (-6)
pcreErrorNosubstring :: Int
pcreErrorNosubstring =  (-7)

-}

pcreInfoCapturecount :: Int
pcreInfoCapturecount =  2

{-
pcreInfoOptions :: Int
pcreInfoOptions =  0
pcreInfoSize :: Int
pcreInfoSize =  1
pcreInfoCapturecount :: Int
pcreInfoCapturecount =  2
pcreInfoBackrefmax :: Int
pcreInfoBackrefmax =  3
pcreInfoFirstchar :: Int
pcreInfoFirstchar =  4
pcreInfoFirsttable :: Int
pcreInfoFirsttable =  5
pcreInfoLastliteral :: Int
pcreInfoLastliteral =  6
-}

