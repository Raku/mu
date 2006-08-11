-- | Low-level FFI
module Judy.Private where

import Foreign
import Data.Word
import Foreign.C.Types
import Foreign.C.String

#include <Judy.h>
#include <stdlib.h>

--type Value = (#type Word_t)
type Value = WordPtr
--type Value = CULong

newtype JError = JError (Ptr ())
--foreign import ccall unsafe "judy_error" judyError :: JError
-- #def JError_t *judy_error(void) { static JError_t err; return &err; }
judyError = JError nullPtr

newtype Frozen a = Frozen a

-- FIXME: I don't think this is the right type as I'll be comparing this with
-- results which are Ptr Value. const seems to return a number and i didnt found
-- a way to create Ptr Value =P
--pjerr :: Value
--pjerr = (#const PJERR)

-- Not sure if it's the best way to get this pointer, but works.
#def void *j_pjerr(void) { return PJERR; }
foreign import ccall unsafe "j_pjerr" pjerr :: Ptr Value

jerr :: Value
jerr = (-1)

-- what do we gain from doing that newtype instead of simply doing: type Judy1Array = () ?
newtype Judy1Array = Judy1Array Judy1Array

type Judy1 = Ptr Judy1Array

#def void *judy1_new(void) { return calloc(1,sizeof(void *)); }

#def void judy1_free(void *ptr) { Judy1FreeArray(ptr, PJE0); }

-- do we really need this judy1_new? or importing judy1_free?
foreign import ccall unsafe judy1_new :: IO (Ptr Judy1)
foreign import ccall unsafe judy1_free :: Ptr Judy1 -> IO ()

foreign import ccall "&judy1_free" judy1_free_ptr :: FunPtr (Ptr Judy1 -> IO ())

-- TODO: import func descriptions from judy manual

foreign import ccall unsafe "Judy1Set" judy1Set :: Ptr Judy1 -> Value -> JError -> IO CInt
foreign import ccall unsafe "Judy1Unset" judy1Unset :: Ptr Judy1 -> Value -> JError -> IO CInt
foreign import ccall unsafe "Judy1Test" judy1Test :: Judy1 -> Value -> JError -> IO CInt
foreign import ccall unsafe "Judy1FreeArray" judy1FreeArray :: Ptr Judy1 -> JError -> IO Value
foreign import ccall unsafe "Judy1Count" judy1Count :: Judy1 -> Value -> Value -> JError -> IO Value

foreign import ccall unsafe "Judy1First" judy1First :: Judy1 -> Ptr Value -> JError -> IO CInt
foreign import ccall unsafe "Judy1Next" judy1Next :: Judy1 -> Ptr Value -> JError -> IO CInt
foreign import ccall unsafe "Judy1Last" judy1Last :: Judy1 -> Ptr Value -> JError -> IO CInt
foreign import ccall unsafe "Judy1Prev" judy1Prev :: Judy1 -> Ptr Value -> JError -> IO CInt

foreign import ccall unsafe "Judy1FirstEmpty" judy1FirstEmpty :: Judy1 -> Ptr Value -> JError -> IO CInt
foreign import ccall unsafe "Judy1NextEmpty" judy1NextEmpty :: Judy1 -> Ptr Value -> JError -> IO CInt
foreign import ccall unsafe "Judy1LastEmpty" judy1LastEmpty :: Judy1 -> Ptr Value -> JError -> IO CInt
foreign import ccall unsafe "Judy1PrevEmpty" judy1PrevEmpty :: Judy1 -> Ptr Value -> JError -> IO CInt


newtype JudyLArray = JudyLArray JudyLArray
type JudyL = Ptr JudyLArray

#def void judyL_free(void *ptr) { JudyLFreeArray(ptr, PJE0); }

foreign import ccall "&judyL_free" judyL_free_ptr :: FunPtr (Ptr JudyL -> IO ())

foreign import ccall unsafe "JudyLIns" judyLIns :: Ptr JudyL -> Value -> JError -> IO (Ptr Value)
foreign import ccall unsafe "JudyLDel" judyLDel :: Ptr JudyL -> Value -> JError -> IO CInt
foreign import ccall unsafe "JudyLGet" judyLGet :: JudyL -> Value -> JError -> IO (Ptr Value)
foreign import ccall unsafe "JudyLCount" judyLCount :: JudyL -> Value -> Value -> JError -> IO Value
foreign import ccall unsafe "JudyLByCount" judyLByCount :: JudyL -> Value -> Ptr Value -> JError -> IO (Ptr Value)

foreign import ccall unsafe "JudyLFreeArray" judyLFreeArray :: Ptr JudyL -> JError -> IO Value
foreign import ccall unsafe "JudyLMemUsed" judyLMemUsed :: JudyL -> IO Value

foreign import ccall unsafe "JudyLFirst" judyLFirst :: JudyL -> Ptr Value -> JError -> IO (Ptr Value)
foreign import ccall unsafe "JudyLNext" judyLNext :: JudyL -> Ptr Value -> JError -> IO (Ptr Value)
foreign import ccall unsafe "JudyLLast" judyLLast :: JudyL -> Ptr Value -> JError -> IO (Ptr Value)
foreign import ccall unsafe "JudyLPrev" judyLPrev :: JudyL -> Ptr Value -> JError -> IO (Ptr Value)

foreign import ccall unsafe "JudyLFirstEmpty" judyLFirstEmpty :: JudyL -> Ptr Value -> JError -> IO CInt
foreign import ccall unsafe "JudyLNextEmpty" judyLNextEmpty :: JudyL -> Ptr Value -> JError -> IO CInt
foreign import ccall unsafe "JudyLLastEmpty" judyLLastEmpty :: JudyL -> Ptr Value -> JError -> IO CInt
foreign import ccall unsafe "JudyLPrevEmpty" judyLPrevEmpty :: JudyL -> Ptr Value -> JError -> IO CInt


newtype JudySLArray = JudySLArray JudySLArray
type JudySL = Ptr JudySLArray


#def void judySL_free(void *ptr) { JudySLFreeArray(ptr, PJE0); }

-- #def void j_fill(char *p, char x, int len) { int i; for (i=len-1; i!=0; i--) *(p++) = x; p = '\0'; }
#def void j_null(char *p) { p = '\0'; }

--foreign import ccall "j_fill" j_fill :: CString -> CChar -> CInt -> IO ()
foreign import ccall "j_null" j_null :: CString -> IO ()

foreign import ccall "&judySL_free" judySL_free_ptr :: FunPtr (Ptr JudySL -> IO ())

foreign import ccall "JudySLIns" judySLIns :: Ptr JudySL -> CString -> JError -> IO (Ptr Value)
foreign import ccall "JudySLDel" judySLDel :: Ptr JudySL -> CString -> JError -> IO CInt
foreign import ccall "JudySLGet" judySLGet :: JudySL -> CString -> JError -> IO (Ptr Value)
foreign import ccall "JudySLFreeArray" judySLFreeArray :: Ptr JudySL -> JError -> IO Value

foreign import ccall unsafe "JudySLFirst" judySLFirst :: JudySL -> CString -> JError -> IO (Ptr Value)
foreign import ccall unsafe "JudySLNext" judySLNext :: JudySL -> CString -> JError -> IO (Ptr Value)
foreign import ccall unsafe "JudySLLast" judySLLast :: JudySL -> CString -> JError -> IO (Ptr Value)
foreign import ccall unsafe "JudySLPrev" judySLPrev :: JudySL -> CString -> JError -> IO (Ptr Value)


newtype JudyHSArray = JudyHSArray JudyHSArray
type JudyHS = Ptr JudyHSArray

#def void judyHS_free(void *ptr) { JudyHSFreeArray(ptr, PJE0); }
#def void judyHSIter_free(void *ptr) { JudyHSFreeIter(ptr, PJE0); }


#def void jp_null(char **p) { p = NULL; }
foreign import ccall "jp_null" jp_null :: Ptr CString -> IO ()



newtype JudyHSIterType = JudyHSIterType JudyHSIterType
type JudyHSIter = Ptr JudyHSIterType

foreign import ccall "&judyHS_free" judyHS_free_ptr :: FunPtr (Ptr JudyHS -> IO ())
foreign import ccall "&judyHSIter_free" judyHSIter_free_ptr :: FunPtr (Ptr JudyHSIter -> IO ())

foreign import ccall "JudyHSIns" judyHSIns :: Ptr JudyHS -> Ptr CChar -> CULong -> JError -> IO (Ptr Value)
foreign import ccall "JudyHSDel" judyHSDel :: Ptr JudyHS -> Ptr CChar -> Value -> JError -> IO CInt
foreign import ccall "JudyHSGet" judyHSGet :: JudyHS -> Ptr CChar -> Value -> IO (Ptr Value)
foreign import ccall "JudyHSFreeArray" judyHSFreeArray :: Ptr JudyHS -> JError -> IO Value

foreign import ccall "JudyHSIterFirst" judyHSIterFirst :: JudyHS -> Ptr JudyHSIter -> Ptr CString -> Ptr Value -> JError -> IO (Ptr Value)
foreign import ccall "JudyHSIterNext" judyHSIterNext :: JudyHS -> Ptr JudyHSIter -> Ptr CString -> Ptr Value -> JError -> IO (Ptr Value) 
foreign import ccall "JudyHSIterLast" judyHSIterLast :: JudyHS -> Ptr JudyHSIter -> Ptr CString -> Ptr Value -> JError -> IO (Ptr Value) 
foreign import ccall "JudyHSIterPrev" judyHSIterPrev :: JudyHS -> Ptr JudyHSIter -> Ptr CString -> Ptr Value -> JError -> IO (Ptr Value) 
foreign import ccall "JudyHSFreeIter" judyHSFreeIter :: Ptr JudyHSIter -> JError -> IO Value

