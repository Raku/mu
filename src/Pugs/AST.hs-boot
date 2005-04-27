{-# OPTIONS_GHC -fglasgow-exts -fno-warn-orphans #-}

{-
    Abstract syntax tree.

    Tall ships and tall kings
    Three times three.
    What brought they from the foundered land
    Over the flowing sea?
    Seven stars and seven stones
    And one white tree.
-}

module Pugs.AST where
import Pugs.Types
import Pugs.Internals

type VScalar = Val
type VList = [Val]
type VSubst = (VRule, Exp)
type VHash = [(VStr, Val)]
type VArray = [Val]
type Eval x = ContT Val (ReaderT Env IO) x
type VPair = (Val, Val)
type VBlock = Exp
type Params = [Param]

data Env
data VCode
data VJunc
data VRef
data VControl
data Param
data Scope

newtype VThunk = MkThunk (Eval Val)
newtype Pad = MkPad (Map Var [IORef VRef])

data Exp
    = Noop
    | App !String ![Exp] ![Exp]
    | Syn !String ![Exp]
    | Cxt !Cxt !Exp
    | Sym !Scope !Var
    | Pad !Scope !Pad
    | Prim !([Val] -> Eval Val)
    | Val !Val
    | Var !Var
    | Parens !Exp
    | NonTerm !SourcePos
    | Stmts ![(Exp, SourcePos)]

data Val

undef :: Val

retError :: VStr -> Exp -> Eval a

data IVar v

newScalar :: (MonadIO m) => VScalar -> m (IVar VScalar)
constScalar :: VScalar -> IVar VScalar
proxyScalar :: Eval VScalar -> (VScalar -> Eval ()) -> IVar VScalar

lazyScalar :: VScalar -> IVar VScalar
lazyUndef :: IVar VScalar

readIVar :: IVar v -> Eval v
writeIVar :: IVar v -> v -> Eval ()

valToStr :: Val -> Eval VStr
