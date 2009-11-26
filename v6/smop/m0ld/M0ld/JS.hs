module M0ld.JS where
import M0ld.AST
import M0ld.M0ld
import Data.List (intersperse)
import qualified M0ld.C
import qualified Data.Map as Map

indent depth code = unlines $ map indentLine $ lines code where
    indentLine "" = ""
    indentLine line = (take depth $ repeat ' ') ++ line

emitStmt regs labels hints (i,c) stmt =
    let emit code = (i+1,c ++ "case "++ (show i) ++ ":\n" ++ indent 2 code)
        reg r = "frame.reg[" ++ (show $ resolveReg r regs) ++ "]" 
        list regs = "[" ++ (concat $ intersperse "," $ map reg regs) ++ "]"
        assign target expr = reg target ++ " = " ++ expr ++ ";\n"
        hint typ reg = Map.lookup (typ,reg) hints
        in
    case stmt of
    -- we can insert special variants here
    Call target identifier capture@(Capture invocant positional named) -> dispatch
        where
        dispatch = emit $ "frame.pc = " ++ (show $ i+1) ++ ";\n" ++
            "frame.ret = " ++ (show $ resolveReg target regs) ++ ";\n" ++
            (reg invocant ++ ".DISPATCH(interpreter," ++ reg identifier ++ ",new P6capture(" ++ 
            list (invocant:positional) ++ "," ++  list named ++ "));") ++ "break;\n"

    --Call2 target responder identifier capture ->
    --    map (\r -> resolveReg r regs) [target,responder,identifier,capture]

    Goto label -> emit $ "frame.pc = "++(show $ resolveLabelDef label labels) ++ ";\n" ++ "break;\n";

    Br value iftrue iffalse ->
        emit $ "frame.pc = " ++
        reg value
         ++
        " == SMOP__NATIVE__bool_false ? " ++
        (show $ resolveLabelDef iffalse labels) ++
        " : " ++
        (show $ resolveLabelDef iftrue labels) ++
        ";\n" ++
        "break;\n"

    LabelDef label -> (i,c)

    Decl reg value -> (i,c)
    
    Hint _ _ _ -> (i,c)

    Assign lvalue rvalue -> emit $ reg lvalue ++ " = " ++ reg rvalue ++ ";\n"

emitFunc regMap labelsMap hints stmts = let 
    (i,cases) = foldl (emitStmt regMap labelsMap hints) (0,"") stmts
    in
    "function(interpreter,frame) {\n" ++
    "  switch (frame.pc) {\n" ++
    indent 4 cases ++
    "case " ++ show i ++ ":" ++ "frame.pc = -1;\n" ++ 
    "  }\n" ++ 
    "}\n"


stmtSize (Decl _ _) = 0
stmtSize (LabelDef _) = 0
stmtSize (Hint _ _ _) = 0
stmtSize _ = 1

mapLabelsToStmts :: [Stmt] -> LabelsMap
mapLabelsToStmts stmts = fst $ foldl addLabelDef (Map.empty,0) stmts
    where
        addLabelDef (labels,offset) (LabelDef label) = (Map.insert label offset labels,offset)
        addLabelDef (labels,offset) stmt = (labels,offset+stmtSize stmt)

extractHints = foldl addHint Map.empty
    where
        addHint hints (Hint typ reg ri) = Map.insert (typ,reg) ri hints
        addHint hints _ = hints

compileToJS stmts =
    let labelsMap = mapLabelsToStmts stmts
        regMap    = mapRegisters stmts
        freeRegs  = countRegister stmts
        hints     = extractHints stmts
        constants = dumpConstantsToJS stmts 
        in "new P6Mold(" ++ show freeRegs ++ "," ++ constants ++ "," ++ (emitFunc regMap labelsMap hints stmts) ++ ")"

dumpConstantsToJS stmts = "[" ++ (concat $ intersperse "," $ concat [dumpConstantToJS c | Decl reg c <- stmts]) ++ "]"

dumpConstantToJS value = case value of
    StringConstant str -> ["new P6Str(\"" ++ str ++ "\")"]
    IntegerConstant int -> ["new P6Int(" ++ show int ++ ")"]
    None -> []
    Var name -> [name]
    SubMold stmts -> [compileToJS stmts]
