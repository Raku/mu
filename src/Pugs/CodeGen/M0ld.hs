{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances -fno-warn-orphans -funbox-strict-fields -cpp #-}

module Pugs.CodeGen.M0ld (genM0ld) where
import Pugs.AST
import Pugs.Compile
import Pugs.PIL1.Instances ()
import Pugs.PIL1
import Control.Monad.State

uniqueId = do
    modify (+1)
    id <- get
    return $ "$id"++(show id)
void = "$void"
placeholder other r = return $ "my "++ r ++ " = " ++ (show other) ++ "; #placeholder\n"

class EmitM0ld a where
    emit :: a -> [Char] -> State Int [Char]

instance EmitM0ld PIL_Environment where
    emit env r = emit (pilMain env) r
instance EmitM0ld PIL_Stmts where
    emit statement r = case statement of
        PNil                           -> return "nil"
        PPad {}                        -> return "pad"
        PStmts{pStmt=stmt,pStmts=PNil} -> emit stmt r
        PStmts{pStmt=stmt,pStmts=rest} -> do 
            stmt <- emit stmt void
            rest <- emit rest void
            return $ stmt ++ rest

instance EmitM0ld PIL_Stmt where
    emit statement r = case statement of 
        PPos {pNode=stmt}  -> emit stmt r
        PNoop              -> return "#noop\n"
        PStmt {pExpr=expr} -> emit expr r

instance EmitM0ld PIL_Expr where
    emit expr r = case expr of 
        PExp {pLV=lv} -> emit lv r
        PCode {pBody=body} -> do
            ret <- uniqueId
            body <- emit body ret
            return ("my " ++ r ++ " = $Code.\"new\"(mold {\n"
                ++ body
                ++ void ++ " = $interpreter.\"return\"(" ++ ret ++ ");\n"
                ++ "});\n")
        PLit {pLit=lit} -> emit lit r
        other -> placeholder other r

instance EmitM0ld PIL_Literal where
    emit PVal {pVal=val} r = emit val r

instance EmitM0ld Val where
    emit (VInt int) r = return $ "my " ++ r ++ " = " ++ (show int) ++ ";\n"
    emit other r = placeholder other r

instance EmitM0ld PIL_LValue where
    emit lvalue r = case lvalue of
        PApp {pFun=fun,pArgs=args,pInv=Nothing} -> do
            fun_r <- uniqueId
            fun_code <- emit fun fun_r
            args <- mapM (\arg -> do
                id <- uniqueId
                code <- emit arg id
                return (code,id)) args
            return (fun_code ++ (concat $ fmap fst args) ++ "my " ++ r ++ " = " ++ fun_r ++ ".\"postcircumfix:( )\"(" ++ (concat $ fmap snd args) ++ ");\n")
        PVar {pVarName=name} -> do
            return $ "my " ++ r ++ " = $scope.\"postcircumfix:{ }\"(\"" ++ name ++ "\");\n"
        other -> return $ (show other) ++ ";\n"

genM0ld :: FilePath -> Eval Val
genM0ld filepath = do
    penv <- compile () :: Eval PIL_Environment
    return $ VStr $ (evalState (emit penv void) 0)
