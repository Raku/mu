{-# OPTIONS_GHC -fglasgow-exts #-}

module PIL.MetaModel where
import PIL.Internals

type Object = Dynamic

newObject :: (Typeable a) => a -> Dynamic
newObject = toDyn

instance Ord Object where
    compare _ _ = EQ
instance Eq Object where
    _ == _ = True
