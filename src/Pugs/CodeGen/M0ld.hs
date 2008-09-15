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

lexicalPrelude = "my $interpreter;\n"
            ++ "my $scope;\n" 
            ++ "my $void;\n"
            ++ "my $Code_scalar = $scope.\"lookup\"(\"Code\");\n"
            ++ "my $Code = $Code_scalar.\"FETCH\"();\n"

instance EmitM0ld PIL_Environment where
    emit env r = do
        main <- emit (pilMain env) r
        return $ lexicalPrelude ++ main
instance EmitM0ld PIL_Stmts where
    emit statement r = case statement of
        PNil                           -> return "nil"
        PPad {}                        -> return "pad"
        PStmts{pStmt=stmt,pStmts=PNil} -> emit stmt r
        PStmts{pStmt=stmt,pStmts=rest} -> do 
            stmt <- emit stmt void
            rest <- emit rest r
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
            return ("my " ++ r ++ " = $Code.\"new\"(:\"outer\"($scope),:\"mold\"(mold {\n"
                ++ lexicalPrelude
                ++ body
                ++ "my $continuation = $interpreter.\"continuation\"();\n"
                ++ "my $back = $continuation.\"back\"();\n"
                ++ "my $void = $back.\"setr\"(" ++ ret ++ ");\n"
                ++ "$void = $interpreter.\"goto\"($back);\n"
--                ++ void ++ " = $interpreter.\"return\"(" ++ ret ++ ");\n"
                ++ "}));\n")
        PLit {pLit=lit} -> emit lit r
        other -> placeholder other r

instance EmitM0ld PIL_Literal where
    emit PVal {pVal=val} r = emit val r

instance EmitM0ld Val where
    emit (VInt int) r = return $ "my " ++ r ++ " = " ++ (show int) ++ ";\n"
    -- " support
    emit (VStr str) r = return $ "my " ++ r ++ " = " ++ (show str) ++ ";\n"
    emit other r = placeholder other r

methodCall inv method args r = do
    inv_r <- uniqueId
    inv_code <- emit inv inv_r
    args <- mapM (\arg -> do
        id <- uniqueId
        code <- emit arg id
        return (code,id)) args
    return (inv_code ++ (concat $ fmap fst args) ++ "my " ++ r ++ " = " ++ inv_r ++ ".\"" ++ method ++ "\"(" ++ (concat $ fmap snd args) ++ ");\n")
instance EmitM0ld PIL_LValue where
    emit lvalue r = case lvalue of
        PApp {pFun=fun,pArgs=args,pInv=Nothing} ->
            methodCall fun "postcircumfix:( )" args r
        PApp {pFun=PExp {pLV = PVar {pVarName = '&':method}},pArgs=args,pInv=Just inv} ->
            methodCall inv method args r
        PVar {pVarName=name} -> do
            scalar <- uniqueId
            return $ "my " ++ scalar ++ " = $scope.\"lookup\"(\"" ++ name ++ "\");\n"
                ++ "my " ++ r ++ " = " ++ scalar ++ ".\"FETCH\"();\n"
        other -> return $ (show other) ++ ";\n"

genM0ld :: FilePath -> Eval Val
genM0ld filepath = do
    penv <- compile () :: Eval PIL_Environment
    return $ VStr $ (evalState (emit penv void) 0)
