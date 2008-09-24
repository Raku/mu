{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances -fno-warn-orphans -funbox-strict-fields -cpp #-}

module Pugs.CodeGen.M0ld (genM0ld) where
import Pugs.AST
import Pugs.Compile
import Pugs.PIL1.Instances ()
import Pugs.PIL1
import Control.Monad.State
import Data.List

uniqueId = do
    modify (+1)
    id <- get
    return $ "$id"++(show id)
void = "$void"
placeholder other r = return $ "my "++ r ++ " = " ++ (show other) ++ "; #placeholder\n"

class EmitM0ld a where
    emit :: a -> [Char] -> State Int [Char]

setupTopmostScope =
       "my $void;\n"
    ++ "my $scope = ?SMOP__S1P__LexicalScope.\"new\"();\n"
    ++ "\n"
    ++ "my $OUT_root_scalar = ?SMOP__S1P__RootNamespace.\"postcircumfix:{ }\"(\"$*OUT\");\n"
    ++ "my $OUT_root = $OUT_root_scalar.\"FETCH\"();\n"
    ++ "my $OUT_scalar = $scope.\"postcircumfix:{ }\"(\"$OUT\");\n"
    ++ "$void = $OUT_scalar.\"STORE\"($OUT_root);\n"
    ++ "\n"

    ++ "my $Code_scalar = $scope.\"postcircumfix:{ }\"(\"Code\");\n"
    ++ "my $Code_root_scalar = ?SMOP__S1P__RootNamespace.\"postcircumfix:{ }\"(\"::Code\");\n"
    ++ "my $Code = $Code_root_scalar.\"FETCH\"();\n"
    ++ "$void = $Code_scalar.\"STORE\"($Code);\n"

    ++ "my $Scalar_scalar = $scope.\"postcircumfix:{ }\"(\"Scalar\");\n"
    ++ "my $Scalar_root_scalar = ?SMOP__S1P__RootNamespace.\"postcircumfix:{ }\"(\"::Scalar\");\n"
    ++ "my $Scalar = $Scalar_root_scalar.\"FETCH\"();\n"
    ++ "$void = $Scalar_scalar.\"STORE\"($Scalar);\n"

    ++ "\n"
    ++ "\n"
    ++ "##############################################################\n"

lexicalPrelude = "my $interpreter;\n"
            ++ "my $scope;\n" 
            ++ "my $void;\n"
            ++ "my $Code_scalar = $scope.\"lookup\"(\"Code\");\n"
            ++ "my $Code = $Code_scalar.\"FETCH\"();\n"
            ++ "my $Scalar_scalar = $scope.\"lookup\"(\"Scalar\");\n"
            ++ "my $Scalar = $Scalar_scalar.\"FETCH\"();\n"

instance EmitM0ld PIL_Environment where
    emit env r = do
        main <- emit (pilMain env) r
        return $ setupTopmostScope ++ main
instance EmitM0ld PIL_Stmts where
    emit statement r = case statement of
        PNil                           -> return "nil"
        PPad {pStmts=rest,pScope=SMy,pSyms=[(var,_)]} -> do
            new_scalar <- uniqueId
            lexpad_cell <- uniqueId
            rest <- emit rest r
            return $
                "my " ++ new_scalar ++ " = $Scalar.\"new\"();\n"
                ++  "my " ++ lexpad_cell ++ " = $scope.\"postcircumfix:{ }\"(" ++ (show var) ++ ");\n"
                ++ "my $void = " ++ lexpad_cell ++ ".\"STORE\"(" ++ new_scalar ++ ");\n"
                ++ rest
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

fetch expr r = do
    scalar <- uniqueId
    expr_code <- expr scalar 
    return $ expr_code ++ "my " ++ r ++ " = " ++ scalar ++ ".\"FETCH\"();\n";

methodCall inv method args r = do
    inv_r <- uniqueId
    inv_code <- fetch (emit inv) inv_r
    args <- mapM (\arg -> do
        arg_reg <- uniqueId
        arg_code <- fetch (emit arg) arg_reg
        return (arg_code,arg_reg)) args
    return (inv_code ++ (concat $ fmap fst args) ++ "my " ++ r ++ " = " ++ inv_r ++ ".\"" ++ method ++ "\"(" ++ (concat $ intersperse "," $ fmap snd args) ++ ");\n")

instance EmitM0ld PIL_LValue where
    emit lvalue r = case lvalue of
        PApp {pFun=fun,pArgs=args,pInv=Nothing} ->
            methodCall fun "postcircumfix:( )" args r
        PApp {pFun=PExp {pLV = PVar {pVarName = '&':method}},pArgs=args,pInv=Just inv} ->
            methodCall inv method args r
        PVar {pVarName=name} -> return $ "my " ++ r ++ " = $scope.\"lookup\"(\"" ++ name ++ "\");\n"
        PAssign {pLHS=[lhs],pRHS=rhs} -> do
            lhs_reg <- uniqueId
            lhs_code <- emit lhs lhs_reg 
            rhs_reg <- uniqueId
            rhs_code <- fetch (emit rhs) rhs_reg
            return $ lhs_code ++ rhs_code ++ "my " ++ r ++ " = " ++ lhs_reg ++ ".\"STORE\"(" ++ rhs_reg ++ ");\n"

        other -> return $ (show other) ++ ";\n"

genM0ld :: FilePath -> Eval Val
genM0ld filepath = do
    penv <- compile () :: Eval PIL_Environment
    return $ VStr $ (evalState (emit penv void) 0)
