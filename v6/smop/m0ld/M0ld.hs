module M0ld where
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

prettyPrintConstant :: [Char] -> Value -> [Char]
prettyPrintConstant indent value = case value of
    StringConstant str -> (show str) ++ "\n"
    IntegerConstant int -> (show int) ++ "\n"
    None -> ""
    Var name -> "¢" ++ name ++ "\n"
    SubMold stmts -> "{\n" ++ (prettyPrintBytecode ("  " ++ indent) stmts) ++ indent ++ "}\n"
