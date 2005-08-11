{-# OPTIONS_GHC -fglasgow-exts #-}

module PIL.Exp where
import PIL.Val
import PIL.Pad
import PIL.Internals

{-

To represent "print 1" as a syntax tree, we need:

- function application
- variable lookup
- literals

-}

data Exp
    = App { appFun :: Exp, appArg :: Exp }
    | Var { varSym :: Sym }
    | Lit { litVal :: Val }
    deriving (Show, Eq, Ord, Typeable)
