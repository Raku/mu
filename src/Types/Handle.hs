module Types.Handle where

import AST
import Internals

type Layer = VStr
type FileDescriptor = VInt

class (Show a) => HandleClass a where
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
