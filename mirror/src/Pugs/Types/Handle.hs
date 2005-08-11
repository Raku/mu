
type Layer = VStr
type FileDescriptor = VInt

class (Typeable a) => HandleClass a where
    handle_iType :: a -> Type
    handle_iType = const $ mkType "IO"
    handle_fetch       :: a -> Eval VHandle
    handle_store       :: a -> VHandle -> Eval ()
    handle_write       :: a -> VStr -> Eval VInt
    handle_write = error ""
    handle_print       :: a -> [Val] -> Eval VBool
    handle_print gv vals = do
        hdl  <- handle_fetch gv
        strs <- mapM valToStr vals
        tryIO False $ do
            hPutStr hdl $ concatMap encodeUTF8 strs
            return True
    handle_printf      :: a -> VStr -> [Val] -> Eval VBool
    handle_printf = error ""
    handle_read        :: a -> VInt -> Eval (VInt, VStr)
    handle_read = error ""
    handle_readLine    :: a -> Eval VStr
    handle_readLine = error ""
    handle_getC        :: a -> Eval VStr
    handle_getC = error ""
    handle_close       :: a -> Eval ()
    handle_close gv = do
        hdl <- handle_fetch gv
        liftIO $ hClose hdl
    handle_binmode     :: a -> Layer -> Eval ()
    handle_binmode _ _ = return ()
    handle_open        :: a -> Layer -> FilePath -> Eval VBool
    handle_open = error ""
    handle_eof         :: a -> Eval VBool
    handle_eof = error ""
    handle_fileNo      :: a -> Eval FileDescriptor
    handle_fileNo = error ""
    handle_seek        :: a -> VInt -> SeekMode -> Eval VBool
    handle_seek = error ""
    handle_tell        :: a -> Eval VInt
    handle_tell = error ""

instance HandleClass IHandle where
    handle_fetch = return
    handle_store = error "store"

