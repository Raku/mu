{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances -fno-warn-orphans -funbox-strict-fields -cpp #-}

module Pugs.CodeGen.M0ld (genM0ld) where
import Pugs.AST
import Pugs.Compile
import Pugs.PIL1.Instances ()
import Pugs.PIL1
import Control.Monad.State

class EmitM0ld a where
    emit :: a -> [Char] -> State Int [Char]

instance EmitM0ld PIL_Environment where
    emit x r = emit (pilMain x) r
instance EmitM0ld PIL_Stmts where
    emit PNil r = return "nil"
    emit PPad {} r = return "pad"
    emit PStmts{pStmt=stmt,pStmts=PNil} r = (emit stmt "void")
    emit PStmts{pStmt=stmt,pStmts=rest} r = do 
        stmt <- emit stmt "void"
        rest <- emit rest "void"
        return $ stmt ++ rest

instance EmitM0ld PIL_Stmt where
    emit PPos {pNode=stmt} = emit stmt
    emit PNoop = \_-> return ";\n"
    emit PStmt {pExpr=expr} = emit expr

instance EmitM0ld PIL_Expr where
    emit PExp {pLV=lv} r = emit lv r
    emit PCode {pBody=body} r =
     do
        body <- emit body "void"
        let ret = "?"
        return ("my $" ++ r ++ " = $Code.\"new\"(mold {\n"
         ++ body
         ++ "$interpreter.\"return\"($" ++ ret ++ ");\n"
         ++ "});\n")

instance EmitM0ld PIL_LValue where
    emit PApp {pFun=fun,pArgs=args,pInv=Nothing} r =
     do
        let fun_r = "?"
        fun <- emit fun fun_r
        return (fun ++ "my $" ++ r ++ " = $" ++ fun_r ++ ".\"postcircumfix:( )\"(" ++ (show args) ++ ");\n")
    emit PApp {pFun=fun,pArgs=args,pInv=Just inv} r =
     do
        inv <- emit inv r
        fun <- emit fun r
        return (inv ++ ".(" ++ fun ++ ")(" ++ (show args) ++ ")")
    emit PVar {pVarName=name} r =
     do
        return $ "my $" ++ r ++ " = $scope.\"postcircumfix:{ }\"(\"" ++ name ++ "\");\n"
    emit x r = return $ show x

genM0ld :: FilePath -> Eval Val
genM0ld filepath = do
    penv <- compile () :: Eval PIL_Environment
    return $ VStr $ (evalState (emit penv "out") 0) ++ "\n"
