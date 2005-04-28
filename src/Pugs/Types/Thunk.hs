
class (Typeable a) => ThunkClass a where
    thunk_iType :: a -> Type
    thunk_iType = const $ mkType "Thunk"
    thunk_force :: a -> Eval Val

instance ThunkClass VThunk where
    thunk_force (MkThunk c) = c

