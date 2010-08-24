module Mildew.AST where
type Lbl = String
data Capture = Capture Expr [Expr] [Expr]
    deriving (Show,Read)
data Expr = 
    Reg String |
    IntegerConstant Int |
    StringConstant String |
    Block [Expr] [String] |
    Call Expr Capture |
    Branch Expr Lbl Lbl |
    Goto  Lbl |
    Label Lbl |
    Assign Expr Expr
    deriving (Show,Read)

