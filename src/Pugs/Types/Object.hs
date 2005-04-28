
class (Typeable a) => ObjectClass a where
    object_iType :: a -> Type

instance (Typeable a) => ObjectClass (IVar a) where
    object_iType (IScalar x) = scalar_iType x
    object_iType (IArray x)  = array_iType x
    object_iType (IHash x)   = hash_iType x
    object_iType (ICode x)   = code_iType x
    object_iType (IHandle x) = handle_iType x
    object_iType (IRule x)   = rule_iType x
    object_iType (IThunk x)  = thunk_iType x
    object_iType (IPair x)   = pair_iType x
