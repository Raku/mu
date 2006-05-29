-- | Low-level FFI
module Judy.Private where

import Foreign
import Data.Word
import Foreign.C.Types

#include <Judy.h>
#include <stdlib.h>

type WordPtr = (#type Word_t)

newtype JError = JError (Ptr ())
--foreign import ccall unsafe "judy_error" judyError :: JError
-- #def JError_t *judy_error(void) { static JError_t err; return &err; }
judyError = JError nullPtr

newtype Frozen a = Frozen a

-- FIXME: I don't think this is the right type as I'll be comparing this with
-- results which are Ptr WordPtr. const seems to return a number and i didnt found
-- a way to create Ptr WordPtr =P
pjerr :: WordPtr
pjerr = (#const PJERR)


-- what do we gain from doing that newtype instead of simply doing: type Judy1Array = ()
newtype Judy1Array = Judy1Array Judy1Array

type Judy1 = Ptr Judy1Array

#def void *judy1_new(void) { return calloc(1,sizeof(void *)); }

#def void judy1_free(void *ptr) { Judy1FreeArray(ptr, PJE0); }

-- do we really need this judy1_new? or importing judy1_free?
foreign import ccall unsafe judy1_new :: IO (Ptr Judy1)
foreign import ccall unsafe judy1_free :: Ptr Judy1 -> IO ()

foreign import ccall "&judy1_free" judy1_free_ptr :: FunPtr (Ptr Judy1 -> IO ())

-- TODO: import func descriptions from judy manual

foreign import ccall unsafe "Judy1Set" judy1Set :: Ptr Judy1 -> WordPtr -> JError -> IO CInt
foreign import ccall unsafe "Judy1Unset" judy1Unset :: Ptr Judy1 -> WordPtr -> JError -> IO CInt
foreign import ccall unsafe "Judy1Test" judy1Test :: Judy1 -> WordPtr -> JError -> IO CInt
foreign import ccall unsafe "Judy1FreeArray" judy1FreeArray :: Ptr Judy1 -> JError -> IO WordPtr
foreign import ccall unsafe "Judy1Count" judy1Count :: Judy1 -> WordPtr -> WordPtr -> JError -> IO WordPtr

foreign import ccall unsafe "Judy1First" judy1First :: Judy1 -> Ptr WordPtr -> JError -> IO CInt
foreign import ccall unsafe "Judy1Next" judy1Next :: Judy1 -> Ptr WordPtr -> JError -> IO CInt
foreign import ccall unsafe "Judy1Last" judy1Last :: Judy1 -> Ptr WordPtr -> JError -> IO CInt
foreign import ccall unsafe "Judy1Prev" judy1Prev :: Judy1 -> Ptr WordPtr -> JError -> IO CInt

foreign import ccall unsafe "Judy1FirstEmpty" judy1FirstEmpty :: Judy1 -> Ptr WordPtr -> JError -> IO CInt
foreign import ccall unsafe "Judy1NextEmpty" judy1NextEmpty :: Judy1 -> Ptr WordPtr -> JError -> IO CInt
foreign import ccall unsafe "Judy1LastEmpty" judy1LastEmpty :: Judy1 -> Ptr WordPtr -> JError -> IO CInt
foreign import ccall unsafe "Judy1PrevEmpty" judy1PrevEmpty :: Judy1 -> Ptr WordPtr -> JError -> IO CInt

newtype JudyLArray = JudyLArray JudyLArray
type JudyL = Ptr JudyLArray

#def void judyL_free(void *ptr) { JudyLFreeArray(ptr, PJE0); }

foreign import ccall "&judyL_free" judyL_free_ptr :: FunPtr (Ptr JudyL -> IO ())

foreign import ccall unsafe "JudyLIns" judyLIns :: Ptr JudyL -> WordPtr -> JError -> IO (Ptr WordPtr)
foreign import ccall unsafe "JudyLDel" judyLDel :: Ptr JudyL -> WordPtr -> JError -> IO CInt
foreign import ccall unsafe "JudyLGet" judyLGet :: JudyL -> WordPtr -> JError -> IO (Ptr WordPtr)
foreign import ccall unsafe "JudyLCount" judyLCount :: JudyL -> WordPtr -> WordPtr -> JError -> IO WordPtr
foreign import ccall unsafe "JudyLByCount" judyLByCount :: JudyL -> WordPtr -> Ptr WordPtr -> JError -> IO (Ptr WordPtr)

foreign import ccall unsafe "JudyLFreeArray" judyLFreeArray :: Ptr JudyL -> JError -> IO WordPtr
foreign import ccall unsafe "JudyLMemUsed" judyLMemUsed :: JudyL -> IO WordPtr

foreign import ccall unsafe "JudyLFirst" judyLFirst :: JudyL -> Ptr WordPtr -> JError -> IO (Ptr WordPtr)
foreign import ccall unsafe "JudyLNext" judyLNext :: JudyL -> Ptr WordPtr -> JError -> IO (Ptr WordPtr)
foreign import ccall unsafe "JudyLLast" judyLLast :: JudyL -> Ptr WordPtr -> JError -> IO (Ptr WordPtr)
foreign import ccall unsafe "JudyLPrev" judyLPrev :: JudyL -> Ptr WordPtr -> JError -> IO (Ptr WordPtr)

foreign import ccall unsafe "JudyLFirstEmpty" judyLFirstEmpty :: JudyL -> Ptr WordPtr -> JError -> IO CInt
foreign import ccall unsafe "JudyLNextEmpty" judyLNextEmpty :: JudyL -> Ptr WordPtr -> JError -> IO CInt
foreign import ccall unsafe "JudyLLastEmpty" judyLLastEmpty :: JudyL -> Ptr WordPtr -> JError -> IO CInt
foreign import ccall unsafe "JudyLPrevEmpty" judyLPrevEmpty :: JudyL -> Ptr WordPtr -> JError -> IO CInt


