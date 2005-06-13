{-# OPTIONS_GHC -fglasgow-exts #-}

module Emit.PIR where
import Text.PrettyPrint

data RegType = RegInt | RegNum | RegStr | RegPMC
    deriving (Show, Eq)

data RelOp = RelLT | RelLE | RelEQ | RelNE | RelGE | RelGT
    deriving (Show, Eq)

data ObjType    = PerlUndef
    deriving (Show, Eq)

type LabelName  = String
type SubName    = String
type VarName    = String

data Identifier
    = Var VarName
    | Reg RegType Int

data Literal
    = LitStr String
    | LitInt Integer
    | LitNum Double

class (Show x) => Emit x where
    emit :: (Monad m) => x -> m Doc
    emit x = fail ("Unrecognized construct: " ++ show x)

data Stmt
    = Comment   String
    | Ins       Instruction
    | Label     LabelName Instruction

data Instruction
    = Local     RegType VarName
    | New       ObjType Identifier
    | Set       Identifier Literal
    | Assign    Identifier Literal

data SubType = SubMAIN | SubLOAD | SubANON | SubMETHOD
    deriving (Show, Eq)

type PIR = [Decl]

data Decl = Sub SubName [SubType]

{-
.sub main @MAIN
    .local pmc s__k

.end
    Comment :: String -> PIR Stmt
    Stmt    :: Maybe Label -> PIR Instruction -> PIR Stmt
    RegHard :: RegType -> Int -> PIR Var
    RegTemp :: RegType -> Int -> PIR Var
    Ident   :: Ident -> PIR Var
    Lit     :: (ConstClass a) => a -> PIR Constant
    Pragma  :: Pragma -> PIR Decl
    Sub     :: Ident -> [PIR Parameter] -> [PIR Stmt] -> PIR Decl
    Emit    :: [PIR PASM] -> PIR Decl
    TypReg  :: RegType -> PIR Typ
    TypPMC  :: Ident -> PIR Typ
    PASM    :: [String] -> PIR PASM
    Param   :: PIR Var -> PIR Parameter
    Return  :: [PIR Var] -> PIR Stmt
    Local   :: PIR Typ -> [Ident] -> PIR Stmt
    Const   :: PIR Typ -> Ident -> PIR Constant -> PIR Stmt
    NS      :: Ident -> [PIR Decl] -> PIR Decl
    Goto    :: Ident -> PIR Stmt
    Cond    :: Bool -> PIR Var -> Ident -> PIR Stmt
    CondCmp :: Bool -> PIR Var -> RelOP -> PIR Var -> Ident -> PIR Stmt
-}
