module Types.Handle where

import {-# SOURCE #-} AST
import Internals

type Layer = VStr
type FileDescriptor = VInt

class Class a where
    fetch       :: a -> Eval VHandle
    store       :: a -> VHandle -> Eval ()
    write       :: a -> VStr -> Eval VInt
    print       :: a -> [Val] -> Eval VBool
    printf      :: a -> VStr -> [Val] -> Eval VBool
    read        :: a -> VInt -> Eval (VInt, VStr)
    readLine    :: a -> Eval VStr
    getC        :: a -> Eval VStr
    close       :: a -> Eval ()
    binmode     :: a -> Layer -> Eval ()
    open        :: a -> Layer -> FilePath -> Eval VBool
    eof         :: a -> Eval VBool
    fileNo      :: a -> Eval FileDescriptor
    seek        :: a -> VInt -> SeekMode -> Eval VBool
    tell        :: a -> Eval VInt
