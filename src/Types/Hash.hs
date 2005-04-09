module Types.Hash where

import AST
import Internals

type Index = VStr

class (Show a) => HashClass a where
    fetch       :: a -> Index -> Eval Val
    fetchKeys   :: a -> Eval [Index] -- XXX Pugs addition
    fetchKeys hv = do
        first <- firstKey hv
        (`fix` first) $ \iter rv -> do
            case rv of
                Nothing -> return []
                Just this -> do
                    next <- nextKey hv this
                    rest <- iter next
                    return (this:rest)
    store       :: a -> Index -> Val -> Eval ()
    delete      :: a -> Index -> Eval ()
    clear       :: a -> Eval ()
    clear hv = do
        keys <- fetchKeys hv
        mapM_ (delete hv) keys
    firstKey    :: a -> Eval (Maybe Index)
    nextKey     :: a -> Index -> Eval (Maybe Index)
    isEmpty     :: a -> Eval VBool

