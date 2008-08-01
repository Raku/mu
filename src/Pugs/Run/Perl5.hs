{-# OPTIONS_GHC -fglasgow-exts -cpp -fno-warn-unused-binds -fno-warn-unused-imports -fallow-overlapping-instances -optc-DSTABLE_H -optc-w #-}

module Pugs.Run.Perl5 () where

#ifdef PUGS_HAVE_PERL5

import Pugs.Internals
import Pugs.AST
import Pugs.Prim.Eval
import Pugs.Embed.Perl5
import Pugs.Types
import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array

foreign export ccall "pugs_Eval"
    pugs_eval :: CString -> IO PugsVal

foreign export ccall "pugs_Apply"
    pugs_apply :: PugsVal -> PugsVal -> Ptr PugsVal -> CInt -> IO PerlSV

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

foreign export ccall "pugs_PvnToVal"
    pvnToVal :: CString -> CInt -> IO PugsVal

foreign export ccall "pugs_PvnToValUTF8"
    pvnToValUTF8 :: CString -> CInt -> IO PugsVal

foreign export ccall "pugs_UndefVal"
    undefVal :: IO PugsVal

askPerl5Env :: IO Env
askPerl5Env = deEnv =<< pugs_getenv

pugs_eval :: CString -> IO PugsVal
pugs_eval cstr = do
    str <- peekCString cstr
    env <- askPerl5Env
    val <- runEvalIO env $ opEval quiet "<eval>" str
    mkValPtr val
    where
    quiet = MkEvalStyle
        { evalResult    = EvalResultLastValue
        , evalError     = EvalErrorUndef
        }

pugs_apply :: PugsVal -> PugsVal -> Ptr PugsVal -> CInt -> IO PerlSV
pugs_apply subPtr invPtr argsPtr cxt = do
    env     <- askPerl5Env
    sub     <- deVal subPtr
    inv     <- deValMaybe invPtr
    args    <- mapM deVal =<< peekArray0 nullVal argsPtr
    let subExp = case sub of
            VStr name@('&':_)   -> _Var name
            VStr name           -> _Var ('&':name)
            _                   -> Val sub
    -- warn "Applying:" (subExp, inv, args, envLexical env)
    val <- runEvalIO env $
        evalExp (Ann (Cxt (cxtEnum cxt)) $
            App subExp (fmap Val inv) (map Val args))
    newSVval val

deVal :: PugsVal -> IO Val
deVal ptr = deRefStablePtr ptr

deEnv :: PugsEnv -> IO Env
deEnv ptr = do
    env <- deRefStablePtr ptr
    return env{ envDebug = Nothing }

nullVal :: PugsVal
nullVal = castPtrToStablePtr nullPtr

deValMaybe :: PugsVal -> IO (Maybe Val)
deValMaybe ptr | castStablePtrToPtr ptr == nullPtr = return Nothing
deValMaybe ptr = fmap Just (deVal ptr)

valToSv :: PugsVal -> IO PerlSV
valToSv ptr = do
    val <- deVal ptr
    newSVval val

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
mkSvRef = mkValPtr . VV . mkVal  -- NewVal/MO
-- mkSvRef = mkValPtr . PerlSV            -- OldVal

ivToVal :: CInt -> IO PugsVal
ivToVal = mkValPtr . VInt . fromIntegral

nvToVal :: CDouble -> IO PugsVal
nvToVal = mkValPtr . VNum . realToFrac

pvnToValUTF8 :: CString -> CInt -> IO PugsVal
pvnToValUTF8 cstr len = do
    str <- peekCStringLen (cstr, fromEnum len)
    mkValPtr $ VStr (decodeUTF8 str)

pvnToVal :: CString -> CInt -> IO PugsVal
pvnToVal cstr len = do
    str <- peekCStringLen (cstr, fromEnum len)
    mkValPtr $ VStr str -- XXX - wrong - make Buf object!

undefVal :: IO PugsVal
undefVal = mkValPtr VUndef

#endif
