{-# OPTIONS_GHC -fglasgow-exts -fallow-overlapping-instances -fallow-undecidable-instances #-}
module Pugs.Prim.FileTest (
    isReadable, isWritable, isExecutable,
    exists, isFile, isDirectory,
    fileSize, sizeIsZero,
    fileMTime, fileCTime, fileATime,
    fileTestViaPerl5
) where
import Pugs.Internals
import Pugs.Embed
import Pugs.Types
import Pugs.AST hiding (isWritable)

-- filetest operators --

-- Officially, these should return a stat object, which sometimes pretends
-- to be a boolean, and may(?) return the filename in string context.
-- DARCS was working on stat, and we should perhaps grab their work:
--  http://www.abridgegame.org/pipermail/darcs-users/2005-February/005499.html
-- They currently (2004-04-05) seem to be using:
--  http://abridgegame.org/cgi-bin/darcs.cgi/darcs/win32/System/Posix.hs
-- For the moment, these return filename and false or undef.
-- Known Bugs: multiple stat()s are done, and filename isnt a boolean.

isReadable   :: Val -> Eval Val
isReadable   = fileTestIO fileTestIsReadable
isWritable   :: Val -> Eval Val
isWritable   = fileTestIO fileTestIsWritable
isExecutable :: Val -> Eval Val
isExecutable = fileTestIO fileTestIsExecutable
exists       :: Val -> Eval Val
exists       = fileTestIO fileTestExists
isFile       :: Val -> Eval Val
isFile       = fileTestIO fileTestIsFile
isDirectory  :: Val -> Eval Val
isDirectory  = fileTestIO fileTestIsDirectory
fileSize     :: Val -> Eval Val
fileSize     = fileTestIO fileTestFileSize
sizeIsZero   :: Val -> Eval Val
sizeIsZero   = fileTestIO fileTestSizeIsZero
fileMTime    :: Val -> Eval Val
fileMTime    = fileTime statFileMTime
fileCTime    :: Val -> Eval Val
fileCTime    = fileTime statFileCTime
fileATime    :: Val -> Eval Val
fileATime    = fileTime statFileATime

fileTestViaPerl5 :: String -> Val -> Eval Val
fileTestViaPerl5 testOp v = do
    env     <- ask
    envSV   <- io $ mkEnv env
    argSV   <- fromVal v
    subSV   <- io $ evalPerl5 ("sub { -" ++ testOp ++ " $_[0] }") envSV (enumCxt cxtItemAny)
    rv      <- runInvokePerl5 subSV nullSV [argSV]
    return $ case rv of
        VStr "" -> VBool False
        VNum 1  -> VBool True
        VInt 1  -> VBool True
        _       -> rv

fileTime :: (FilePath -> IO Integer) -> Val -> Eval Val
fileTime test f = do
    t   <- fileTestIO (fileTestDo test) f
    if (t == undef) then return VUndef else do
    t'  <- fromVal t :: Eval Integer
    b   <- fromVal =<< readVar (cast "$*BASETIME")
    return $ VRat $ (b - (pugsTimeSpec . posixSecondsToUTCTime $ fromIntegral t')) / 86400

fileTestIO :: (Value n) => (n -> IO Val) -> Val -> Eval Val
fileTestIO f v = do
    str <- fromVal =<< fromVal' v
    tryIO undef $ f str

valFromBool :: Value a => a -> Bool -> Val
valFromBool v b = if b then castV v else VBool False

testPerms :: (Permissions -> Bool) -> FilePath -> IO Val
testPerms t f = do
    p <- getPermissions f
    let b = t p
    return $ valFromBool f b

fileTestIsReadable :: FilePath -> IO Val
fileTestIsReadable = testPerms readable

fileTestIsWritable :: FilePath -> IO Val
fileTestIsWritable = testPerms writable

fileTestIsExecutable :: FilePath -> IO Val
fileTestIsExecutable = testPerms $ liftM2 (||) executable searchable

fileTestExists :: FilePath -> IO Val
fileTestExists f = doesExist f >>= return . (valFromBool f)

fileTestIsFile :: FilePath -> IO Val
fileTestIsFile f = doesFileExist f >>= return . (valFromBool f)

fileTestIsDirectory :: FilePath -> IO Val
fileTestIsDirectory f = doesDirectoryExist f >>= return . (valFromBool f)

fileTestFileSize :: FilePath -> IO Val
fileTestFileSize f = statFileSize f >>= return . VInt

fileTestSizeIsZero :: FilePath -> IO Val
fileTestSizeIsZero f = do
    n <- statFileSize f
    return $ if n == 0 then VBool True else VBool False

fileTestDo :: (FilePath -> IO Integer) -> FilePath -> IO Val
fileTestDo test f = test f >>= return . VInt
