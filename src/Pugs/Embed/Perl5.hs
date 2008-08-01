{-# OPTIONS_GHC -fglasgow-exts -cpp -optc-w #-}

#ifndef PUGS_HAVE_PERL5
module Pugs.Embed.Perl5 
    ( InvokePerl5Result(..)
    , svToVBool, svToVInt, svToVNum, svToVStr, vstrToSV, vintToSV, svToVal, bufToSV, svUndef
    , vnumToSV, mkValRef, mkValPtr, mkEnv, PerlSV, nullSV, nullEnv, evalPerl5, invokePerl5
    , initPerl5, freePerl5, canPerl5
    , evalPCR, pugs_SvToVal
    )
where
import Foreign.C.Types
import System.Directory
import Pugs.Internals 
import qualified Data.ByteString.UTF8 as Str

evalPCR :: FilePath -> String -> String -> [(String, String)] -> IO String
evalPCR path match rule subrules = do
    (inp, out, err, pid) <- initPCR path
    (`mapM` subrules) $ \(name, rule) -> do
        let nameStr = escape name
            ruleStr = escape rule
        hPutStrLn inp $ unwords
            ["add_rule", show (length nameStr), show (length ruleStr)]
        hPutStrLn inp nameStr
        hPutStrLn inp ruleStr
    let matchStr = escape match
        ruleStr  = escape rule
    hPutStrLn inp $ unwords
        ["match", show (length matchStr), show (length ruleStr)]
    hPutStrLn inp $ matchStr
    hPutStrLn inp $ ruleStr
    hFlush inp
    rv <- hGetLine out
    case rv of
        ('O':'K':' ':_) -> do
            -- size <- readIO sizeStr
            -- rv   <- sequence (replicate size (hGetChar out))
            ln   <- hGetLine out
            ln2  <- hGetLine out
            return $ ln ++ ln2
        _ -> do
            errMsg  <- fmap (rv ++) (hGetContents err)
            rc      <- waitForProcess pid
            writeIORef _Perl5Interp Nothing
            let msg | null errMsg = show rc
                    | otherwise   = errMsg
            fail $ "*** Running external 'perl' failed:\n" ++ msg
    where
    escape "" = ""
    escape ('\\':xs) = "\\\\" ++ escape xs
    escape ('\n':xs) = "\\n" ++ escape xs
    escape (x:xs) = (x:escape xs)

initPCR :: FilePath -> IO Perl5Interp
initPCR path = do
    rv <- readIORef _Perl5Interp
    case rv of
        Just interp@(_, _, _, pid) -> do
            gone <- getProcessExitCode pid
            if isNothing gone then return interp else do
            writeIORef _Perl5Interp Nothing
            initPCR path
        Nothing -> do
            cmd <- findPerl5
            interp <- runInteractiveProcess cmd
                [ "-Iperl5/Pugs-Compiler-Rule/lib"
                , "-MPugs::Runtime::Match::HsBridge"
                , "-ePugs::Runtime::Match::HsBridge::__CMD__"
                ] (Just path) Nothing 
            writeIORef _Perl5Interp (Just interp)
            return interp
    where
    findPerl5 :: IO FilePath
    findPerl5 = do
        rv <- findExecutable "perl"
        case rv of
            Nothing     -> do
                rv' <- findExecutable "perl.exe"
                case rv' of
                    Just cmd    -> return cmd
                    Nothing     -> fail "Cannot find the parrot executable in PATH"
            Just cmd    -> return cmd

type Perl5Interp = (Handle, Handle, Handle, ProcessHandle)

{-# NOINLINE _Perl5Interp #-}
_Perl5Interp :: IORef (Maybe Perl5Interp)
_Perl5Interp = unsafePerformIO $ newIORef Nothing

type PerlInterpreter = ()
data PerlSV = MkPerlSV -- phantom type
    deriving (Show, Eq, Ord, Typeable)
type PugsVal = PerlSV
type PugsEnv = PerlSV

data InvokePerl5Result
    = Perl5ReturnValues [PerlSV]
    | Perl5ErrorString String
    | Perl5ErrorObject PerlSV
    deriving (Show, Typeable)

constFail :: a -> IO b
constFail = const $ fail "perl5 not embedded"

initPerl5 :: String -> Maybe a -> IO PerlInterpreter
initPerl5 _ _ = return ()

freePerl5 :: PerlInterpreter -> IO ()
freePerl5 _ = return ()

evalPerl5 :: String -> PugsEnv -> CInt -> IO PerlSV
evalPerl5 _ _ = constFail

svToVStr :: PerlSV -> IO String
svToVStr = constFail

svToVInt :: (Num a) => PerlSV -> IO a
svToVInt = constFail

svToVNum :: (Fractional a) => PerlSV -> IO a
svToVNum = constFail

svToVBool :: PerlSV -> IO Bool
svToVBool = constFail

svToVal :: PerlSV -> IO a
svToVal = constFail

mkValPtr :: (Show a) => a -> IO PugsVal
mkValPtr = constFail

mkEnv :: a -> IO PugsVal
mkEnv = constFail

mkValRef :: a -> String -> IO PerlSV
mkValRef _ = constFail

vstrToSV :: String -> IO PerlSV
vstrToSV = constFail

svUndef :: IO PerlSV
svUndef = error "perl5 not embedded"

bufToSV :: ByteString -> IO PerlSV
bufToSV = constFail

vintToSV :: (Integral a) => a -> IO PerlSV
vintToSV = constFail

vnumToSV :: (Real a) => a -> IO PerlSV
vnumToSV = constFail

invokePerl5 :: PerlSV -> PerlSV -> [PerlSV] -> PugsEnv -> CInt -> IO InvokePerl5Result
invokePerl5 _ _ _ _ = constFail

canPerl5 :: PerlSV -> ByteString -> IO Bool
canPerl5 MkPerlSV = constFail

pugs_SvToVal :: PerlSV -> IO PugsVal
pugs_SvToVal = constFail

nullSV :: PerlSV
nullSV = error "perl5 not embedded"

nullEnv :: PugsVal
nullEnv = error "perl5 not embedded"

-- Below are unused

-- mkSV :: IO PerlSV -> IO PerlSV
-- mkSV = id

-- perl5_SvROK :: IO PerlSV -> IO Bool
-- perl5_SvROK _ = return False

#else
#undef RETURN

{-# INCLUDE "../../perl5/p5embed.h" #-}
{-# INCLUDE "../../perl5/pugsembed.h" #-}

module Pugs.Embed.Perl5 where
import Pugs.Internals
import Foreign
import Foreign.C.Types
import Foreign.C.String
import {-# SOURCE #-} Pugs.AST.Internals
import qualified Data.ByteString.UTF8 as Str
import qualified Data.ByteString.Char8 as Buf
import qualified Pugs.Val as Val

type PerlInterpreter = Ptr ()
type PerlSV = Ptr ()
type PugsVal = StablePtr Val
type PugsEnv = StablePtr Env

foreign import ccall "perl_alloc"
    perl_alloc :: IO PerlInterpreter
foreign import ccall "perl_construct"
    perl_construct :: PerlInterpreter -> IO ()
foreign import ccall "perl_run"
    perl_run :: PerlInterpreter -> IO CInt
foreign import ccall "perl_destruct"
    perl_destruct :: PerlInterpreter -> IO CInt
foreign import ccall "perl_free"
    perl_free :: PerlInterpreter -> IO ()
{-
foreign import ccall "boot_DynaLoader"
    boot_DynaLoader :: Ptr () -> IO ()
-}
foreign import ccall "perl5_finalize"
    perl5_finalize :: PerlSV -> IO ()
foreign import ccall "perl5_SvPV"
    perl5_SvPV :: PerlSV -> IO CString
foreign import ccall "perl5_SvIV"
    perl5_SvIV :: PerlSV -> IO CInt
foreign import ccall "perl5_SvNV"
    perl5_SvNV :: PerlSV -> IO CDouble
foreign import ccall "perl5_SvTRUE"
    perl5_SvTRUE :: PerlSV -> IO Bool
foreign import ccall "perl5_SvROK"
    perl5_SvROK :: PerlSV -> IO Bool
foreign import ccall "perl5_newSVpvn"
    perl5_newSVpvn :: CString -> CInt -> IO PerlSV
foreign import ccall "perl5_newSViv"
    perl5_newSViv :: CInt -> IO PerlSV
foreign import ccall "perl5_newSVnv"
    perl5_newSVnv :: CDouble -> IO PerlSV
foreign import ccall "perl5_sv_undef"
    perl5_sv_undef :: IO PerlSV
foreign import ccall "perl5_get_sv"
    perl5_get_sv :: CString -> IO PerlSV
foreign import ccall "perl5_apply"
    perl5_apply :: PerlSV -> PerlSV -> Ptr PerlSV -> PugsEnv -> CInt -> IO (Ptr PerlSV)
foreign import ccall "perl5_can"
    perl5_can :: PerlSV -> CString -> IO Bool
foreign import ccall "perl5_eval"
    perl5_eval :: CString -> PugsEnv -> CInt -> IO PerlSV
foreign import ccall "perl5_init"
    perl5_init :: CInt -> Ptr CString -> IO PerlInterpreter

foreign import ccall "pugs_getenv"
    pugs_getenv :: IO PugsEnv
foreign import ccall "pugs_setenv"
    pugs_setenv :: PugsEnv -> IO ()

foreign import ccall "pugs_SvToVal"
    pugs_SvToVal :: PerlSV -> IO PugsVal
foreign import ccall "pugs_MkValRef"
    pugs_MkValRef :: PugsVal -> CString -> IO PerlSV

initPerl5 :: String -> Maybe Env -> IO PerlInterpreter
initPerl5 str env = do
    withCString "-e" $ \prog -> withCString str $ \cstr -> do
        withArray [prog, prog, cstr] $ \argv -> do
            interp <- perl5_init 3 argv
            case env of
                Just val    -> pugs_setenv =<< mkEnv val
                Nothing     -> return ()
            modifyIORef _GlobalFinalizer (>> perl_free interp)
            return interp

mkValPtr :: Val -> IO PugsVal
mkValPtr x = do
    -- warn "Creating nonblessed stable pointer for " (showVal x)
    newStablePtr x

mkEnv :: Env -> IO PugsEnv
mkEnv = newStablePtr

svToVStr :: PerlSV -> IO String
svToVStr sv = peekCString =<< perl5_SvPV sv

svToVInt :: (Num a) => PerlSV -> IO a
svToVInt sv = fmap fromIntegral $ perl5_SvIV sv

svToVNum :: (Fractional a) => PerlSV -> IO a
svToVNum sv = fmap realToFrac $ perl5_SvNV sv

svToVBool :: PerlSV -> IO Bool
svToVBool = perl5_SvTRUE

svToVal :: PerlSV -> IO Val
svToVal sv = do
    ptr <- pugs_SvToVal sv
    deRefStablePtr ptr

mkValRef :: Val -> String -> IO PerlSV
mkValRef x typ = do
    -- warn "Creating stable pointer for " (showVal x)
    val <- mkValPtr x
    withCString typ (pugs_MkValRef val)

svUndef :: IO PerlSV
svUndef = perl5_sv_undef

vstrToSV :: String -> IO PerlSV
vstrToSV str = Buf.useAsCStringLen (cast str) $ \(cstr, len) -> perl5_newSVpvn cstr (toEnum len)

bufToSV :: ByteString -> IO PerlSV
bufToSV str = Buf.useAsCStringLen str $ \(cstr, len) -> perl5_newSVpvn cstr (toEnum len)

vintToSV :: (Integral a) => a -> IO PerlSV
vintToSV int = perl5_newSViv (fromIntegral int)

vnumToSV :: (Real a) => a -> IO PerlSV
vnumToSV int = perl5_newSVnv (realToFrac int)


data InvokePerl5Result
    = Perl5ReturnValues [PerlSV]
    | Perl5ErrorString String
    | Perl5ErrorObject PerlSV
    deriving (Show, Typeable)

invokePerl5 :: PerlSV -> PerlSV -> [PerlSV] -> PugsEnv -> CInt -> IO InvokePerl5Result
invokePerl5 sub inv args env cxt = do
    withArray0 nullPtr args $ \argv -> do
        rv  <- perl5_apply sub inv argv env cxt
        svs <- peekArray0 nullPtr rv

        -- If it's empty, no error occured (see p5embed.c on out[0]).
        -- Otherwise, the second slot may be a stringified version we should use.
        case svs of
            []      -> fmap Perl5ReturnValues $ peekArray0 nullPtr (rv `advancePtr` 1)
            [err]   -> return $ Perl5ErrorObject err
            (_:x:_) -> do
                str <- svToVStr x
                return $ Perl5ErrorString str
            
canPerl5 :: PerlSV -> ByteString -> IO Bool
canPerl5 sv meth = Buf.useAsCString meth $ \cstr -> perl5_can sv cstr

mkSV :: IO PerlSV -> IO PerlSV
mkSV action = action
{-
 - do
    sv <- action 
    addFinalizer sv (perl5_finalize sv)
    return sv
-}

evalPerl5 :: String -> PugsEnv -> CInt -> IO PerlSV
evalPerl5 str env cxt = mkSV $ Buf.useAsCString (cast str) $ \cstr -> perl5_eval cstr env cxt

freePerl5 :: PerlInterpreter -> IO ()
freePerl5 my_perl = do
    perl_destruct my_perl
    return ()

nullSV :: PerlSV
nullSV = nullPtr

{-# NOINLINE nullEnv #-}
nullEnv :: PugsEnv
nullEnv = unsafePerformIO (newStablePtr (error "undefined environment"))

evalPCR :: FilePath -> String -> String -> [(String, String)] -> IO String
evalPCR path match rule subrules = do
    let bridgeMod   = "Pugs::Runtime::Match::HsBridge"
        bridgeFile  = "Pugs/Runtime/Match/HsBridge.pm";
    inv     <- evalPerl5 (unlines
        [ "if (!$INC{'"++bridgeFile++"'}) {"
        , "    unshift @INC, '"++path++"';"
        , "    eval q[require '"++bridgeFile++"'] or die $@;"
        , "}"
        , "'"++bridgeMod++"'"
        ]) nullEnv 1
    meth    <- vstrToSV "__RUN__"
    args    <- mapM vstrToSV $ concatMap (\(x, y) -> [x, y]) ((match, rule):subrules)
    rv      <- invokePerl5 meth inv args nullEnv 1
    case rv of
        Perl5ReturnValues []    -> return ""
        Perl5ReturnValues (x:_) -> svToVStr x
        Perl5ErrorString err    -> return $ "Error: " ++ err
        Perl5ErrorObject obj    -> do
            err <- svToVStr obj
            return $ "Error: " ++ err

#endif

