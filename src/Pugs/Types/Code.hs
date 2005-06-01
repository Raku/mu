
class (Typeable a) => CodeClass a where
    code_iType    :: a -> Type
    code_iType = const $ mkType "Code"
    code_fetch    :: a -> Eval VCode
    code_fetch a = code_assuming a [] []
    code_store    :: a -> VCode -> Eval ()
    code_assuming :: a -> [Exp] -> [Exp] -> Eval VCode
    code_apply    :: a -> Eval Val
    code_assoc    :: a -> VStr
    code_params   :: a -> Params
    code_type     :: a -> SubType

instance CodeClass ICode where
    code_iType c  = code_iType . unsafePerformSTM $ readTVar c
    code_fetch    = liftSTM . readTVar
    code_store    = (liftSTM .) . writeTVar
    code_assuming c [] [] = code_fetch c
    code_assuming _ _ _   = undefined
    code_apply    = error "apply"
    code_assoc c  = code_assoc . unsafePerformSTM $ readTVar c
    code_params c = code_params . unsafePerformSTM $ readTVar c
    code_type c   = code_type . unsafePerformSTM $ readTVar c

instance CodeClass VCode where
    -- XXX - subType should really just be a mkType itself
    code_iType c  = case subType c of
        SubBlock    -> mkType "Block"
        SubMethod   -> mkType "Method"
        _           -> mkType "Sub"
    code_fetch    = return
    code_store c _= retConstError $ VStr $ show c
    code_assuming c [] [] = return c
    code_assuming _ _ _   = error "assuming"
    code_apply    = error "apply"
    code_assoc    = subAssoc
    code_params   = subParams
    code_type     = subType

