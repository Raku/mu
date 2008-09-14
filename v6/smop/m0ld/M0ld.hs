import Text.ParserCombinators.Parsec hiding (label)
import qualified Data.Map as Map
import System.IO hiding (getContents,putStrLn,print,putStr)
import Prelude hiding (getContents,putStrLn,print,putStr)
import System.IO.UTF8
import Debug.Trace
import System.Console.GetOpt
import System.Environment

type Register = [Char]
type Label = [Char]
data Value = Var [Char] | IntegerConstant Integer | StringConstant [Char] | None | SubMold [Stmt]
    deriving (Show,Eq,Ord)
data Capture = Capture Register [Register] [Register]
    deriving (Show,Eq,Ord)
data Stmt = LabelDef Label | Decl Register Value | Goto Label | Br Register Label Label | Call Register Register Capture | Call2 Register Register Register Register  
    deriving (Show,Eq,Ord)
data Argument = Pos Register | Named Register Register

data Mold = Mold [Stmt]
    deriving Show


identifier = do
    first <- choice [letter, char '_']
    rest <- many $ choice [alphaNum, char '_', char '!']
    return $ [first] ++ rest

ws = do
    many1 $ choice
        [ oneOf "\t\n "
        , char '#' >> many (noneOf "\n") >> newline
        ]
    return [()]

opt_ws = option [()] ws

tok r = do
    res <- r
    opt_ws
    return res

symbol x = tok $ string x

parenthesized = between (symbol "(") (symbol ")")

inBraces = between (symbol "{") (symbol "}")

register =  char '$' >> identifier

label = do
    id <- tok $ identifier
    symbol ":"
    return [LabelDef id]

stmt = do 
    l <- option [] (try label)
    body <- choice $ map try [label,call2,call,decl,goto,br]
    return $ l ++ body

constant = choice 
      [ do
          char '¢'
          name <- identifier
          return $ Var name
      , do
        digits <- many1 digit 
        return $ IntegerConstant $ read digits
      , do
        content <- between (char '"') (char '"') quotedChar
        return $ StringConstant $ concat content
      , submold
      ]
      where
      quotedChar = many $
        do
        c <- noneOf "\"\\"
        return [c]
        <|> do 
            char '\\'
            c <- anyChar
            return ['\\',c]

-- implicit_decl :: GenParser Char ImplicitDecls [Char]
implicit_decl = do
    c <- constant
    decls <- getState
    case (Map.lookup c decls) of
        Just c -> return c
        Nothing ->
            do 
                let new = "___implicit_register___"++(show $ Map.size decls)
                updateState $ Map.insert c new
                decls <- getState
                return $ new

value = tok $ choice [register,implicit_decl]

decl = do 
    string "my"
    ws
    x <- tok register
    defaultValue <- option None $ symbol "=" >> constant
    return [Decl x defaultValue]

branch = inBraces $ do
    string "goto"
    ws
    label <- tok identifier
    option ' ' $ char ';'
    return label

br = do
    string "if"
    ws
    cond <- value
    iftrue <- branch
    symbol "else"
    iffalse <- branch
    return [Br cond iftrue iffalse]

goto = do
    string "goto"
    ws
    label <- identifier
    return [Goto label]

call = do
    inline_decl <- option False (symbol "my" >> return True)
    target <- value
    symbol "="
    invocant <- value
    char '.'
    identifier <- value
    arguments <- parenthesized $ sepBy (tok argument) (symbol ",")
    let pos = [ x | Pos x <- arguments]
        named = [x | (Named k v) <- arguments, x <- [k,v]]
        decl = if inline_decl then [Decl target None] else []
        call = [Call target identifier (Capture invocant pos named)]
    return $ decl ++ call

call2 = do
    target <- value
    symbol "="
    responder <- value
    char '.'
    identifier <- value
    capture <- parenthesized $ symbol "|" >> value
    return [Call2 target responder identifier capture]

argument = do
        char ':'
        k <- value
        v <- parenthesized value
        return $ Named k v
    <|> do 
        arg <- value
        return $ Pos arg

terminator = opt_ws >> ((symbol ";" >> return ()) <|> eof)
top = do 
    opt_ws
    stmts <- tok $ endBy stmt terminator
    eof
    constants <- getState
    return (concat $ stmts,constants)

submold = do
    savedState <- getState
    setState Map.empty

    symbol "mold"
    stmts <- inBraces $ tok $ endBy stmt terminator
    constants <- getState
    setState savedState
    return $ SubMold $ (implicitDecls constants) ++ (concat stmts)

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
    Decl reg value -> Map.insert reg (Map.size regs)  regs
    _ -> regs

addFreeRegister :: RegMap -> Stmt -> RegMap
addFreeRegister regs stmt = case stmt of
    Decl reg None -> Map.insert reg (Map.size regs)  regs
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
prettyPrintBytecode indent stmts =
    let labelsMap = mapLabels stmts
        regMap    = mapRegisters stmts
        freeRegs  = countRegister stmts
        prettyPrintOp (Decl _ _) = ""
        prettyPrintOp op = indent ++ (joinStr " " $ ( map show (toBytecode op regMap labelsMap))) ++ "\n"
        decls = [prettyPrintConstant indent c | Decl reg c <- filter (not . isReg) stmts]
        in (concat $ map (\(i,e) -> indent ++ "$" ++ (show i) ++ " = " ++ e) (zip [0..(length decls - 1)] decls)) ++
            (concat $ map prettyPrintOp stmts)

type ImplicitDecls = Map.Map Value [Char]

implicitDecls = map (\(constant,reg) -> Decl reg constant) . Map.toList

main = do
    args <- getArgs
    let (options,nonoptions,errors) =  getOpt RequireOrder [Option [] ["print-bytecode"] (NoArg "print-bytecode") "print resulting mold bytecode in a human readable form"] args 
    mapM putStr errors
    hFlush stdout
    line <- getContents
    case (runParser top (Map.empty :: ImplicitDecls) "" line) of 
        Left err      -> error  $ show err
        Right (stmts,constants) -> do 
            if elem "print-bytecode" options then putStrLn $ prettyPrintBytecode "" $ (implicitDecls constants) ++ stmts
                else putStrLn $ dumpToC $ (implicitDecls constants) ++ stmts
