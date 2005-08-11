{-# OPTIONS_GHC -fglasgow-exts #-}

module PIL.PIL where
import PIL.Val
import PIL.Pad
import PIL.Exp
import PIL.Internals

-- XXX - of course wrong!
data PIL
    = App { appFun :: PIL, appArg :: PIL }
    | Var { varSym :: Sym }
    | Lit { litVal :: Val }
    deriving (Show, Eq, Ord, Typeable)
