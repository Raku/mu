{-# OPTIONS_GHC -fglasgow-exts #-}

module Types.Handle where

import {-# SOURCE #-} AST
import Internals

type Layer = VStr
type FileDescriptor = VInt

class Class a where
    iType :: a -> String
    iType _ = "IO"
    fetch       :: a -> Eval VHandle
    store       :: a -> VHandle -> Eval ()
    write       :: a -> VStr -> Eval VInt
    write = error ""
    print       :: a -> [Val] -> Eval VBool
    print gv vals = do
        hdl  <- fetch gv
        strs <- mapM valToStr vals
        tryIO False $ do
            hPutStr hdl $ concatMap encodeUTF8 strs
            return True
    printf      :: a -> VStr -> [Val] -> Eval VBool
    printf = error ""
    read        :: a -> VInt -> Eval (VInt, VStr)
    read = error ""
    readLine    :: a -> Eval VStr
    readLine = error ""
    getC        :: a -> Eval VStr
    getC = error ""
    close       :: a -> Eval ()
    close gv = do
        hdl <- fetch gv
        liftIO $ hClose hdl
    binmode     :: a -> Layer -> Eval ()
    binmode _ _ = return ()
    open        :: a -> Layer -> FilePath -> Eval VBool
    open = error ""
    eof         :: a -> Eval VBool
    eof = error ""
    fileNo      :: a -> Eval FileDescriptor
    fileNo = error ""
    seek        :: a -> VInt -> SeekMode -> Eval VBool
    seek = error ""
    tell        :: a -> Eval VInt
    tell = error ""
