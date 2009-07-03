module M0ld.M0ld where
--(module M0ld.AST,dumpToC,parseM0ld) where
import M0ld.AST
import M0ld.Parser
import Debug.Trace
import qualified Data.Map as Map

prettyPrintBytecode indent stmts =
    let labelsMap = mapLabels stmts
        regMap    = mapRegisters stmts
        freeRegs  = countRegister stmts
        prettyPrintOp (Decl _ _) = ""
        prettyPrintOp op = indent ++ (joinStr " " $ ( map show (toBytecode op regMap labelsMap))) ++ "\n"
        decls = [prettyPrintConstant indent c | Decl reg c <- filter (not . isReg) stmts]
        in trace (show regMap) (concat $ map (\(i,e) -> indent ++ "$" ++ (show i) ++ " = " ++ e) (zip [0..(length decls - 1)] decls))
        ++ (concat $ map prettyPrintOp stmts)

type RegMap = Map.Map [Char] Int

type LabelsMap = Map.Map [Char] Int

resolveReg r regs = Map.findWithDefault (error $ "undeclared register: $"++r) r regs 

resolveLabelDef l labels = Map.findWithDefault (error $ "undeclared label: "++l) l labels

toBytecode :: Stmt -> RegMap -> LabelsMap -> [Int]
toBytecode stmt regs labels = case stmt of
    Call target identifier (Capture invocant positional named) ->
        let reg r = resolveReg r regs
            args x = [length x] ++ map reg x
            in [1,reg target,reg invocant,reg identifier] ++ args positional ++ args named

    Call2 target responder identifier capture ->
        map (\r -> resolveReg r regs) [target,responder,identifier,capture]

    Goto label -> [3, resolveLabelDef label labels]

    Br value iftrue iffalse ->
        [4,resolveReg value regs,resolveLabelDef iftrue labels,resolveLabelDef iffalse labels]

    LabelDef label -> []

    Decl reg value -> []

    Assign lvalue rvalue -> [5,resolveReg lvalue regs,resolveReg rvalue regs]

isReg (Decl _ None) = True
isReg _ = False

countRegister stmts = length $ filter isReg stmts

addRegister :: RegMap -> Stmt -> RegMap
addRegister regs stmt = case stmt of 
    Decl reg None  -> regs
    Decl reg value -> if (Map.member reg regs) then regs else  Map.insert reg (Map.size regs)  regs
    _ -> regs

addFreeRegister :: RegMap -> Stmt -> RegMap
addFreeRegister regs stmt = case stmt of
    Decl reg None -> if (Map.member reg regs) then regs else Map.insert reg (Map.size regs)  regs
    Decl reg _ -> regs 
    _ -> regs

mapRegisters :: [Stmt] -> RegMap
mapRegisters stmts = foldl addFreeRegister (foldl addRegister Map.empty stmts) stmts

bytecodeLength :: Stmt -> Int
bytecodeLength stmt = case stmt of
    Br _ _ _ -> 4
    Goto _ -> 2
    Call target identifier (Capture invocant positional named) ->
        6 + length positional + length named
    Call2 _ _ _ _ -> 5
    Decl _ _ -> 0
    LabelDef _ -> 0
    Assign _ _ -> 3

addLabelDef (labels,offset) (LabelDef label) = (Map.insert label offset labels,offset)
addLabelDef (labels,offset) stmt = (labels,offset+bytecodeLength stmt)

mapLabels :: [Stmt] -> LabelsMap
mapLabels stmts = fst $ foldl addLabelDef (Map.empty,0) stmts

emit :: [Stmt] -> RegMap -> LabelsMap -> [Int]
emit stmts regMap labelsMap = concatMap (\op -> toBytecode op regMap labelsMap) stmts ++ [0]

joinStr sep [] = ""
joinStr sep list = foldl (\a b -> a ++ sep ++ b) (head list) (tail list)

cStrLength ('\\':next:rest) = 1 + cStrLength rest
cStrLength (c:rest) = 1 + cStrLength rest
cStrLength [] = 0

dumpConstantToC :: Value -> [Char]
dumpConstantToC value = case value of
    StringConstant str ->
        "SMOP__NATIVE__idconst_createn(\"" ++ str ++"\"," ++ (show $ cStrLength str) ++ "),"
    IntegerConstant int -> "SMOP__NATIVE__int_create(" ++ show int ++ "),"
    None -> ""
    Var name -> "SMOP_REFERENCE(interpreter," ++ name ++ "),"
    SubMold stmts -> dumpToC stmts ++ ","

dumpConstantsToC stmts = "(SMOP__Object*[]) {" ++
    concat [dumpConstantToC c | Decl reg c <- stmts] ++ "NULL}"

dumpToC stmts =
    let labelsMap = mapLabels stmts
        regMap    = mapRegisters stmts
        freeRegs  = countRegister stmts
        bytecode  = emit stmts regMap labelsMap
        constants = dumpConstantsToC stmts
        in "SMOP__Mold_create(" ++ show freeRegs ++ "," ++ constants ++ ","
        ++ show (length bytecode) ++ ",(int[]) {"
        ++ (joinStr "," $ map show bytecode)
        ++ "})"

wrap a b (f,c,p) = (f,a++c++b,p)

dumpLOSTConstantsToC prefix stmts = 
    wrap "(SMOP__Object*[]) {" "NULL}" $ foldl dumpLOSTConstantToC ([],"",prefix) stmts

dumpLOSTConstantToC (f,c,p) (Decl reg (SubMold stmts)) = let 
    (f',c',p') = compileToLOST p stmts in
    (f++f',c++c'++",",p')

dumpLOSTConstantToC  (f,c,p) (Decl reg constant) = (f,c++dumpConstantToC constant,p)

dumpLOSTConstantToC fcp _ = fcp 



indent depth code = unlines $ map indentLine $ lines code where
    indentLine "" = ""
    indentLine line = (take depth $ repeat ' ') ++ line

emitLOSTStmt regs labels (i,c) stmt =
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

emitLOSTBody (prefix,id) regMap labelsMap stmts = let 
    name = prefix++(show id) in
    ("static void " ++ name ++ "(SMOP__Object* interpreter,SMOP__LOST__Frame* frame) {\n" ++
    "  switch (frame->pc) {\n" ++
    (indent 4 $ snd $ foldl (emitLOSTStmt regMap labelsMap) (0,"") stmts) ++
    "  }\n" ++ 
    "}\n",name,(prefix,id+1))


stmtSize (Decl _ _) = 0
stmtSize (LabelDef _) = 0
stmtSize _ = 1

mapLabelsLOST :: [Stmt] -> LabelsMap
mapLabelsLOST stmts = fst $ foldl addLabelDefLOST (Map.empty,0) stmts

addLabelDefLOST (labels,offset) (LabelDef label) = (Map.insert label offset labels,offset)
addLabelDefLOST (labels,offset) stmt = (labels,offset+stmtSize stmt)

compileToLOST prefix stmts =
    let labelsMap = mapLabelsLOST stmts
        regMap    = mapRegisters stmts
        freeRegs  = countRegister stmts
        (functions,constants,prefix') = dumpLOSTConstantsToC prefix stmts 
        (funcBody,funcName,prefix'') = emitLOSTBody prefix' regMap labelsMap stmts
        in (funcBody:functions,"SMOP__LOST_create(" ++ show freeRegs ++ "," ++ constants ++ ","
        ++ funcName ++ "})",prefix'')

prettyPrintConstant :: [Char] -> Value -> [Char]
prettyPrintConstant indent value = case value of
    StringConstant str -> (show str) ++ "\n"
    IntegerConstant int -> (show int) ++ "\n"
    None -> ""
    Var name -> "¢" ++ name ++ "\n"
    SubMold stmts -> "{\n" ++ (prettyPrintBytecode ("  " ++ indent) stmts) ++ indent ++ "}\n"
