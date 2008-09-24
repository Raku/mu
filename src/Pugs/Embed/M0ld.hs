{-# LANGUAGE ForeignFunctionInterface,TypeSynonymInstances #-}
module Pugs.Embed.M0ld (evalM0ld) where 

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Debug.Trace
import M0ld
import M0ld.AST
import M0ld.Parser

foreign import ccall "smop.h smop_init"
      smop_init :: IO ()
foreign import ccall "smop.h smop_destr"
      smop_destr :: IO ()
type SMOP__Object = Ptr ()
type Object = ForeignPtr ()

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
idconst_ptr str = unsafePerformIO $ withCStringLen str  (\cstr -> return $ c_SMOP__NATIVE__idconst_createn (fst cstr) (snd cstr))
idconst str = unsafePerformIO $ auto_release $ idconst_ptr str 

foreign import ccall "smop_native.h SMOP__NATIVE__capture_create"
    c_SMOP__NATIVE__capture_create :: SMOP__Object -> SMOP__Object -> Ptr SMOP__Object -> Ptr SMOP__Object -> SMOP__Object

capture_create :: SMOP__Object -> [SMOP__Object] -> [SMOP__Object] -> SMOP__Object
capture_create inv pos named = unsafePerformIO $ (withArray0 nullPtr pos (\cpos -> withArray0 nullPtr named (\cnamed -> return $ c_SMOP__NATIVE__capture_create interpreter inv cpos cnamed)))

foreign import ccall "smop_mold.h SMOP__Mold_create"
    c_SMOP__Mold_create :: Int -> Ptr SMOP__Object -> Int -> Ptr Int -> IO SMOP__Object


foreign import ccall "smop_haskell_ffi.h &smop_release_with_global"
    p_release :: FunPtr (Ptr a -> IO ())

foreign import ccall "smop_haskell_ffi.h smop_get_cvar"
    smop_get_cvar :: CString -> IO SMOP__Object

get_cvar str = do
    obj <- withCString str smop_get_cvar
    auto_release obj

auto_release :: SMOP__Object -> IO Object
auto_release ptr = do 
    c_smop_reference interpreter ptr
    newForeignPtr p_release ptr

foreign import ccall "smop_haskell_ffi.h get_SMOP__S1P__RootNamespace"
      get_SMOP__S1P__RootNamespace :: IO SMOP__Object

rootnamespace = do
    root <- get_SMOP__S1P__RootNamespace
    auto_release root

class Smopify a where
    smopify :: a -> IO SMOP__Object


instance Smopify Object where
    smopify a = withForeignPtr a (\ptr -> do
        c_smop_reference interpreter ptr 
        return ptr)
instance Smopify SMOP__Object where
    smopify a = return a
instance Smopify String where
    smopify a = return (idconst_ptr a)

mold :: (Smopify a) => Int -> [a] -> [Int] -> IO Object
mold regs constants opcodes = do
    constants_ <- mapM smopify constants
    new_mold <- withArray0 nullPtr constants_ (\c_constants -> withArray opcodes (\c_opcodes -> c_SMOP__Mold_create regs c_constants (length opcodes) c_opcodes))
    auto_release new_mold

call inv ident pos named = do
    inv_ <- smopify inv
    ident_ <- smopify ident
    pos_ <- mapM smopify pos
    named_ <- mapM smopify named
    obj <- c_smop_dispatch interpreter (c_smop_ri inv_) ident_ (capture_create inv_ pos_ named_)
    auto_release obj

-- Hack to satisfy the type system
none :: [SMOP__Object]
none = []

metachars :: [Char] -> [Char]
metachars str = case str of
    [] -> ""
    '\\':'n':rest -> '\n' : (metachars rest)
    '\\':other:rest -> other : (metachars rest)
    letter:rest -> letter : (metachars rest)

createConstant :: Value -> IO Object
createConstant constant = case constant of
    Var var -> get_cvar var
    IntegerConstant int -> error "integer constant"
    StringConstant str -> return $ idconst $ metachars $ str
    SubMold stmts -> createM0ld stmts

compileM0ld = createM0ld . parseM0ld

createM0ld ast = do
    let labelsMap = mapLabels ast
        regMap    = mapRegisters ast
        freeRegs  = countRegister ast
        bytecode  = emit ast regMap labelsMap
    constants <- mapM createConstant [c | Decl reg c <- filter (not . isReg) ast]
    mold freeRegs constants bytecode
evalM0ld code = do
    smop_init
    root <- rootnamespace

    out_scalar <- call root "postcircumfix:{ }" ["$*OUT"] none
    out <- call out_scalar "FETCH" none none

    mold_frame_scalar <- call root "postcircumfix:{ }" ["::MoldFrame"] none
    mold_frame <- call mold_frame_scalar "FETCH" none none

    test_mold <- compileM0ld code
    test_frame <- call mold_frame "new" [test_mold] none

    c_smop_reference interpreter interpreter
    call interpreter "goto" [test_frame] none

    c_smop_reference interpreter interpreter
    call interpreter "loop" none none
--    smop_destr
    return ()
