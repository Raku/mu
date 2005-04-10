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

module AST where
import Internals

type VBool = Bool
type VInt  = Integer
type VRat  = Rational
type VNum  = Double
type VScalar = Val
type VComplex = Complex VNum
type VStr  = String
type VList = [Val]
type VSubst = (VRule, Exp)
type VHandle = Handle
type VSocket = Socket
type VThread = ThreadId
type MVal = IORef Val
type VArray = [Val]
type VHash = [(VStr, Val)]
type Eval x = ContT Val (ReaderT Env IO) x
type VPair = (Val, Val)
type VBlock = Exp
type Var = String
type Params = [Param]

data Env
data VCode
data VRule
data VJunc
data VControl
data Param

newtype VThunk = MkThunk (Eval Val)

data Symbol a

data Exp
    = App String [Exp] [Exp]
    | Syn String [Exp]
    | Sym [Symbol Exp]
    | Prim ([Val] -> Eval Val)
    | Val Val
    | Var Var
    | Parens Exp
    | NonTerm SourcePos
    | Statements [(Exp, SourcePos)]

data Val

undef :: Val

retError :: VStr -> Exp -> Eval a

data IVar v

newScalar :: (MonadIO m) => VScalar -> m (IVar VScalar)
constScalar :: VScalar -> IVar VScalar

readIVar :: IVar v -> Eval v
writeIVar :: IVar v -> v -> Eval ()

valToStr :: Val -> Eval VStr
