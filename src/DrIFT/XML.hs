{-# OPTIONS_GHC -fglasgow-exts #-}

import Pugs.AST.Internals
import Text.XML.HaXml.Haskell2Xml

instance (Typeable a) => Haskell2Xml (TVar a) where
    toHType   _    = Prim "TVar" "tvar"
    toContents _   = [CElem (Elem "tvar" [] [])]

instance Haskell2Xml Exp where
    toHType   _    = Prim "Exp" "exp"
    toContents _   = [CElem (Elem "exp" [] [])]
    fromContents _ = (Noop, [])

instance Haskell2Xml Double where
    toHType   _    = Prim "Rational" "rational"
    toContents i   = [CElem (Elem "rational" [mkAttr "value" (show i)] [])]
        where
        mkAttr :: String -> String -> Attribute
        mkAttr n v = (n, AttValue [Left v])
    fromContents (CElem (Elem "rational" [("value",(AttValue [Left s]))] []):cs)
                   = (read s, cs)
    fromContents (_:cs) = fromContents cs
