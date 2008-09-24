{-# LANGUAGE ForeignFunctionInterface #-}
module Pugs.Embed.M0ld (evalM0ld) where 

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Debug.Trace
import M0ld

foreign import ccall "smop.h smop_init"
      smop_init :: IO ()
foreign import ccall "smop.h smop_destr"
      smop_destr :: IO ()
type SMOP__Object = Ptr ()

foreign import ccall "smop_haskell_ffi.h get_SMOP__GlobalInterpreter"
      interpreter :: SMOP__Object

foreign import ccall "smop_haskell_ffi.h smop_reference"
      c_smop_reference :: SMOP__Object -> SMOP__Object -> IO SMOP__Object
foreign import ccall "smop_haskell_ffi.h smop_release"
      c_smop_release :: SMOP__Object -> SMOP__Object -> IO SMOP__Object
foreign import ccall "smop_haskell_ffi.h smop_dispatch"
      c_smop_dispatch :: SMOP__Object -> SMOP__Object -> SMOP__Object -> SMOP__Object -> IO SMOP__Object
foreign import ccall "smop_haskell_ffi.h smop_ri"
      c_smop_ri :: SMOP__Object -> SMOP__Object

foreign import ccall "smop_native.h SMOP__NATIVE__idconst_createn"
      c_SMOP__NATIVE__idconst_createn :: Ptr CChar -> Int -> SMOP__Object
idconst str = unsafePerformIO $ withCStringLen str  (\cstr -> return $ c_SMOP__NATIVE__idconst_createn (fst cstr) (snd cstr))

foreign import ccall "smop_native.h SMOP__NATIVE__capture_create"
    c_SMOP__NATIVE__capture_create :: SMOP__Object -> SMOP__Object -> Ptr SMOP__Object -> Ptr SMOP__Object -> SMOP__Object

capture_create :: SMOP__Object -> [SMOP__Object] -> [SMOP__Object] -> SMOP__Object
capture_create inv pos named = unsafePerformIO $ (withArray0 nullPtr pos (\cpos -> withArray0 nullPtr named (\cnamed -> return $ c_SMOP__NATIVE__capture_create interpreter inv cpos cnamed)))



-- foreign import ccall "smop_haskell_ffi.h &smop_release_with_global"
--    p_release :: FunPtr (Ptr a -> IO ())
-- auto_release ptr = newForeignPtr p_release ptr

foreign import ccall "smop_haskell_ffi.h get_SMOP__S1P__RootNamespace"
      get_SMOP__S1P__RootNamespace :: IO SMOP__Object

rootnamespace = get_SMOP__S1P__RootNamespace
    --obj <- get_SMOP__S1P__RootNamespace
    -- c_smop_reference interpreter obj
    -- auto_release obj

call inv ident pos named = c_smop_dispatch interpreter (c_smop_ri inv) ident (capture_create inv pos named)


evalM0ld code = do
    smop_init
    root <- rootnamespace
    scalar <- call root (idconst "postcircumfix:{ }") [idconst "$*OUT"] []
    io <- call scalar (idconst "FETCH") [] []
    call io (idconst "print") [idconst (dumpToC $ parseM0ld code)] []
    smop_destr
