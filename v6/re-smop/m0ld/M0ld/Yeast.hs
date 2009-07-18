module M0ld.Yeast where
import M0ld.AST
import M0ld.M0ld
import qualified M0ld.C
import qualified Data.Map as Map
wrap a b (f,c,p) = (f,a++c++b,p)

indent depth code = unlines $ map indentLine $ lines code where
    indentLine "" = ""
    indentLine line = (take depth $ repeat ' ') ++ line

emitStmt regs labels hints (i,c) stmt =
    let emit code = (i+1,c ++ "case "++ (show i) ++ ":\n" ++ indent 2 code) 
        reg r = "frame->reg[" ++ (show $ resolveReg r regs) ++ "]" 
        list regs = "(SMOP__Object*[]) {" ++ (concat $ map (\r -> "SMOP_REFERENCE(interpreter," ++ reg r ++ "),") regs) ++ "NULL}"
        assign target expr = "if (" ++ reg target ++ ") SMOP_RELEASE(interpreter," ++ reg target ++ ");\n" ++ reg target ++ " = " ++ expr ++ ";\n"
        hint typ reg = Map.lookup (typ,reg) hints
        in
    case stmt of
    Call target identifier capture@(Capture invocant positional named) -> case hint RI invocant of
        Just (StringConstant "capture") -> case hint Constant identifier of
            Just (StringConstant "positional") -> case capture of
                (Capture invocant [i] []) -> emit $ assign target $ "SMOP__NATIVE__capture_positional(interpreter," ++ reg invocant ++ ",SMOP__NATIVE__int_fetch(" ++ reg i ++ "))"
            _ -> emit identifier
        _ -> dispatch
        where
        dispatch = emit $ 
            "frame->pc = " ++ (show $ i+1) ++ ";\n" ++
            "frame->ret = &" ++ reg target ++ ";\n" ++
            (assign target $ "SMOP_DISPATCH(\n" ++ (indent 2 $ "interpreter,\nSMOP_RI(" ++ reg invocant ++ "),\n" ++
                reg identifier ++ 
                ",\nSMOP__NATIVE__capture_create(interpreter," ++
                    list (invocant:positional) ++ ","
                    ++ list named ++
                ")\n" ) ++ ")") ++
            "break;\n"

    --Call2 target responder identifier capture ->
    --    map (\r -> resolveReg r regs) [target,responder,identifier,capture]

    Goto label -> emit $ "frame->pc = "++(show $ resolveLabelDef label labels) ++ ";\n" ++ "break;\n";

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
    
    Hint _ _ _ -> (i,c)

    Assign lvalue rvalue -> emit $ reg lvalue ++ " = " ++ reg rvalue ++ ";\n"

emitFunc (prefix,id) regMap labelsMap hints stmts = let 
    (i,cases) = foldl (emitStmt regMap labelsMap hints) (0,"") stmts
    name = prefix++(show id) in
    ("static void " ++ name ++ "(SMOP__Object* interpreter,SMOP__Yeast__Frame* frame) {\n" ++
    "  switch (frame->pc) {\n" ++
    indent 4 cases ++
    "case " ++ show i ++ ":" ++ "frame->pc = -1;\n" ++ 
    "  }\n" ++ 
    "}\n",name,(prefix,id+1))


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

compileToYeast prefix stmts =
    let labelsMap = mapLabelsToStmts stmts
        regMap    = mapRegisters stmts
        freeRegs  = countRegister stmts
        hints     = extractHints stmts
        (functions,constants,prefix') = dumpConstantsToC prefix stmts 
        (funcBody,funcName,prefix'') = emitFunc prefix' regMap labelsMap hints stmts
        in (funcBody:functions,"SMOP__Yeast_create(" ++ show freeRegs ++ "," ++ constants ++ ","
        ++ funcName ++ ")",prefix'')

dumpConstantsToC prefix stmts = 
    wrap "(SMOP__Object*[]) {" "NULL}" $ foldl dumpConstantToC ([],"",prefix) stmts

dumpConstantToC (f,c,p) (Decl reg (SubMold stmts)) = let 
    (f',c',p') = compileToYeast p stmts in
    (f++f',c++c'++",",p')

dumpConstantToC  (f,c,p) (Decl reg constant) = (f,c++M0ld.C.dumpConstantToC constant,p)

dumpConstantToC fcp _ = fcp 
