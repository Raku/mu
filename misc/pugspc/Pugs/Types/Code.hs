
class (Typeable a) => CodeClass a where
    code_iType    :: a -> Type
    code_iType = const $ mkType "Code"
    code_fetch    :: a -> Eval VCode
    code_fetch a = code_assuming a [] []
    code_store    :: a -> VCode -> Eval ()
    code_assuming :: a -> [Exp] -> [Exp] -> Eval VCode
    code_apply    :: a -> Eval Val
    code_assoc    :: a -> SubAssoc
    code_params   :: a -> Params
    code_type     :: a -> SubType
    code_clone    :: a -> STM a

instance CodeClass VMultiCode where
    code_iType      = mc_type
    code_fetch c    = do
        -- warn "XXX - Multi dispatch -- Not yet!" ()
        fromVal =<< readVar (Set.findMax (mc_variants c))
    code_store c _  = retConstError . VStr $ show c
    code_params     = mc_signature
    code_assuming c [] [] = code_fetch c
    code_assuming _ _ _   = error "assuming"
    code_assoc      = mc_assoc
    code_apply      = error "apply"
    code_type       = mc_subtype
    code_clone      = return

instance CodeClass VCode where
    -- XXX - subType should really just be a mkType itself
    code_iType c  = case subType c of
        SubBlock    -> mkType "Block"
        SubPointy   -> mkType "Block"
        SubMethod   -> mkType "Method"
        _           -> mkType "Sub"
    code_fetch    = return
    code_store c _= retConstError . VStr $ show c
    code_assuming c [] [] = return c
    code_assuming _ _ _   = error "assuming"
    code_apply    = error "apply"
    code_assoc    = subAssoc
    code_params   = subParams
    code_type     = subType
    code_clone c  = return c

