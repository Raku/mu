module M0ld.C where
import M0ld.M0ld
import M0ld.AST

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
