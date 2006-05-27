
module Judy.Private where

import Foreign
import Data.Word
import Foreign.C.Types

#include <Judy.h>
#include <stdlib.h>

type WordPtr = (#type Word_t)

newtype JudyArray = JudyArray JudyArray
type Judy = Ptr JudyArray

newtype JError = JError (Ptr ())

newtype Frozen a = Frozen a

--foreign import ccall unsafe "judy_error" judyError :: JError

-- #def JError_t *judy_error(void) { static JError_t err; return &err; }

judyError = JError nullPtr

#def void *judy_new(void) { return calloc(1,sizeof(void *)); }

#def void judy1_free(void *ptr) { Judy1FreeArray(ptr, PJE0); }

foreign import ccall unsafe judy_new :: IO (Ptr Judy)
foreign import ccall unsafe judy1_free :: Ptr Judy -> IO ()

foreign import ccall "&judy1_free" judy1_free_ptr :: FunPtr (Ptr Judy -> IO ())


foreign import ccall unsafe "Judy1Set" judy1Set :: Ptr Judy -> WordPtr -> JError -> IO CInt
foreign import ccall unsafe "Judy1Unset" judy1Unset :: Ptr Judy -> WordPtr -> JError -> IO CInt
foreign import ccall unsafe "Judy1Test" judy1Test :: Judy -> WordPtr -> JError -> IO CInt
foreign import ccall unsafe "Judy1FreeArray" judy1FreeArray :: Ptr Judy -> JError -> IO WordPtr
foreign import ccall unsafe "Judy1Count" judy1Count :: Judy -> WordPtr -> WordPtr -> JError -> IO WordPtr
-- forward search
foreign import ccall unsafe "Judy1First" judy1First :: Judy -> Ptr WordPtr -> JError -> IO CInt
foreign import ccall unsafe "Judy1Next" judy1Next :: Judy -> Ptr WordPtr -> JError -> IO CInt
foreign import ccall unsafe "Judy1Last" judy1Last :: Judy -> Ptr WordPtr -> JError -> IO CInt
foreign import ccall unsafe "Judy1Prev" judy1Prev :: Judy -> Ptr WordPtr -> JError -> IO CInt

-- backwards search
foreign import ccall unsafe "Judy1FirstEmpty" judy1FirstEmpty :: Judy -> Ptr WordPtr -> JError -> IO CInt
foreign import ccall unsafe "Judy1NextEmpty" judy1NextEmpty :: Judy -> Ptr WordPtr -> JError -> IO CInt
foreign import ccall unsafe "Judy1LastEmpty" judy1LastEmpty :: Judy -> Ptr WordPtr -> JError -> IO CInt
foreign import ccall unsafe "Judy1PrevEmpty" judy1PrevEmpty :: Judy -> Ptr WordPtr -> JError -> IO CInt



