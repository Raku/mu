module M0ld.LOST where
import M0ld.AST
import M0ld.M0ld
import qualified M0ld.C
import qualified Data.Map as Map
wrap a b (f,c,p) = (f,a++c++b,p)

indent depth code = unlines $ map indentLine $ lines code where
    indentLine "" = ""
    indentLine line = (take depth $ repeat ' ') ++ line

emitStmt regs labels (i,c) stmt =
    let emit code = (i+1,c ++ "case "++ (show i) ++ ":\n" ++ indent 2 code) 
        reg r = "frame->registers[" ++ (show $ resolveReg r regs) ++ "]" in
    case stmt of
    Call target identifier (Capture invocant positional named) -> emit "frame->pc++;\ncall;\nbreak \n"
        --let reg r = resolveReg r regs
        --    args x = [length x] ++ map reg x
        --    in [1,reg target,reg invocant,reg identifier] ++ args positional ++ args named

    --Call2 target responder identifier capture ->
    --    map (\r -> resolveReg r regs) [target,responder,identifier,capture]

    Goto label -> emit $ "frame->pc = "++(show $ resolveLabelDef label labels) ++ "\n"

    Br value iftrue iffalse ->
        emit $ "frame->pc = " ++
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

    Assign lvalue rvalue -> emit $ reg lvalue ++ " = " ++ reg rvalue ++ ";\n"

emitFunc (prefix,id) regMap labelsMap stmts = let 
    name = prefix++(show id) in
    ("static void " ++ name ++ "(SMOP__Object* interpreter,SMOP__LOST__Frame* frame) {\n" ++
    "  switch (frame->pc) {\n" ++
    (indent 4 $ snd $ foldl (emitStmt regMap labelsMap) (0,"") stmts) ++
    "  }\n" ++ 
    "}\n",name,(prefix,id+1))


stmtSize (Decl _ _) = 0
stmtSize (LabelDef _) = 0
stmtSize _ = 1

mapLabelsToStmts :: [Stmt] -> LabelsMap
mapLabelsToStmts stmts = fst $ foldl addLabelDef (Map.empty,0) stmts
    where
        addLabelDef (labels,offset) (LabelDef label) = (Map.insert label offset labels,offset)
        addLabelDef (labels,offset) stmt = (labels,offset+stmtSize stmt)

compileToLOST prefix stmts =
    let labelsMap = mapLabels stmts
        regMap    = mapRegisters stmts
        freeRegs  = countRegister stmts
        (functions,constants,prefix') = dumpConstantsToC prefix stmts 
        (funcBody,funcName,prefix'') = emitFunc prefix' regMap labelsMap stmts
        in (funcBody:functions,"SMOP__LOST_create(" ++ show freeRegs ++ "," ++ constants ++ ","
        ++ funcName ++ "})",prefix'')
dumpConstantsToC prefix stmts = 
    wrap "(SMOP__Object*[]) {" "NULL}" $ foldl dumpConstantToC ([],"",prefix) stmts

dumpConstantToC (f,c,p) (Decl reg (SubMold stmts)) = let 
    (f',c',p') = compileToLOST p stmts in
    (f++f',c++c'++",",p')

dumpConstantToC  (f,c,p) (Decl reg constant) = (f,c++M0ld.C.dumpConstantToC constant,p)

dumpConstantToC fcp _ = fcp 
