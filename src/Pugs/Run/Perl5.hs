{-# OPTIONS_GHC -fglasgow-exts -cpp -fno-warn-unused-binds -fno-warn-unused-imports #-}

module Pugs.Run.Perl5 () where

#ifdef PUGS_HAVE_PERL5

import Pugs.Internals
import Pugs.AST
import Pugs.Prim.Eval
import Pugs.Embed.Perl5
import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array

foreign export ccall "pugs_Eval"
    pugs_eval :: CString -> IO PugsVal

foreign export ccall "pugs_Apply"
    pugs_apply :: PugsVal -> PugsVal -> Ptr PugsVal -> IO PugsVal

foreign export ccall "pugs_ValToSv"
    valToSv :: PugsVal -> IO PerlSV

foreign export ccall "pugs_ValToIv"
    valToIv :: PugsVal -> IO CInt

foreign export ccall "pugs_ValToNv"
    valToNv :: PugsVal -> IO CDouble

foreign export ccall "pugs_ValToPv"
    valToPv :: PugsVal -> IO CString

foreign export ccall "pugs_MkSvRef"
    mkSvRef :: PerlSV -> IO PugsVal

foreign export ccall "pugs_IvToVal"
    ivToVal :: CInt -> IO PugsVal

foreign export ccall "pugs_NvToVal"
    nvToVal :: CDouble -> IO PugsVal

foreign export ccall "pugs_PvToVal"
    pvToVal :: CString -> IO PugsVal

askPerl5Env :: IO Env
askPerl5Env = do
    sv  <- withCString "pugs::env" perl5_get_sv 
    val <- svToVal sv
    case val of
        VControl (ControlEnv env)   -> return env
        _                           -> fail "cannot fetch $pugs::env"

pugs_eval :: CString -> IO PugsVal
pugs_eval cstr = do
    str <- peekCString cstr
    env <- askPerl5Env
    val <- runEvalIO env $ opEval Nothing "<eval>" str
    mkVal val

pugs_apply :: PugsVal -> PugsVal -> Ptr PugsVal -> IO PugsVal
pugs_apply subPtr invPtr argsPtr = do
    env     <- askPerl5Env
    sub     <- deVal subPtr
    inv     <- deValMaybe invPtr
    args    <- mapM deVal =<< peekArray0 nullPtr argsPtr
    let subExp = case sub of
            VStr name   -> Var name
            _           -> Val sub
    val <- runEvalIO env $ evalExp (App subExp (fmap Val inv) (map Val args))
    mkVal val

mkVal :: Val -> IO PugsVal
mkVal val = fmap castStablePtrToPtr $ newStablePtr val

deVal :: PugsVal -> IO Val
deVal ptr = deRefStablePtr (castPtrToStablePtr ptr)

deValMaybe :: PugsVal -> IO (Maybe Val)
deValMaybe ptr | ptr == nullPtr = return Nothing
deValMaybe ptr = fmap Just (deVal ptr)

valToSv :: PugsVal -> IO PerlSV
valToSv ptr = do
    val <- deVal ptr
    case val of
        PerlSV sv   -> return sv
        _           -> mkValRef val

valToIv :: PugsVal -> IO CInt
valToIv ptr = do
    val     <- deVal ptr
    env     <- askPerl5Env
    VInt x  <- runEvalIO env $ fmap VInt (fromVal val)
    return $ fromInteger x

valToNv :: PugsVal -> IO CDouble
valToNv ptr = do
    val     <- deVal ptr
    env     <- askPerl5Env
    VRat x  <- runEvalIO env $ fmap VInt (fromVal val)
    return $ fromRational x

valToPv :: PugsVal -> IO CString
valToPv ptr = do
    val     <- deVal ptr
    env     <- askPerl5Env
    VStr x  <- runEvalIO env $ fmap VInt (fromVal val)
    newCString x

mkSvRef :: PerlSV -> IO PugsVal
mkSvRef = mkVal . PerlSV

ivToVal :: CInt -> IO PugsVal
ivToVal = mkVal . VInt . fromIntegral

nvToVal :: CDouble -> IO PugsVal
nvToVal = mkVal . VNum . realToFrac

pvToVal :: CString -> IO PugsVal
pvToVal = (mkVal . VStr =<<) . peekCString

#endif
