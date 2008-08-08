import Text.ParserCombinators.Parsec hiding (label)
import qualified Data.Map as Map
import System.IO
import Debug.Trace
type Label = [Char]
type Register = [Char]
data Value = Var [Char] | IntegerConstant Integer | StringConstant [Char] | None
    deriving Show
data Capture = Capture Register [Register] [Register]
    deriving Show
data Stmt = Labeled Label Stmt | Decl Register Value | Goto Label | Br Register Label Label | Call Register Register Capture | Call2 Register Register Register Register
    deriving Show
data Argument = Pos Register | Named Register Register

data Mold = Mold [Stmt]
    deriving Show

identifier = do
    first <- choice [alphaNum, char '_']
    rest <- many1 $ choice [alphaNum, char '_', digit]
    return $ [first] ++ rest

ws = do
    many1 $ choice
        [ oneOf "\t\n "
        , char '#' >> many1 (noneOf "\n") >> newline
        ]
    return [()]

opt_ws = option [()] ws

tok r = do
    res <- r
    opt_ws
    return res

lparen = char '('
rparen = char ')'

register =  char '$' >> identifier

label = do
    id <- tok $ identifier
    tok $ char ':'
    return id
stmt =  do
        l <- try label
        labeled <- stmt
        return $ Labeled l labeled
    <|> do 
        choice [(try call2),call,decl,goto,br]

value = choice 
      [ do
          char '$'
          name <- identifier
          return $ Var name
      , do
        digits <- many1 digit 
        return $ IntegerConstant $ read digits
      , do
        content <- between (char '"') (char '"') quotedChar
        return $ StringConstant $ concat content
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

decl = do 
    string "my"
    ws
    x <- tok register
    value <- option None $ (tok $ char '=') >> value
    return (Decl x value)

branch = do
    tok $ char '{'
    string "goto"
    ws
    label <- tok $identifier
    option ' ' $ char ';'
    tok $ char '}'
    return label
br = do
    string "if"
    ws
    cond <- tok register
    iftrue <- branch
    tok $ string "else"
    iffalse <- branch
    return (Br cond iftrue iffalse)
goto = do
    string "goto"
    ws
    label <- identifier
    return (Goto label)

call = do
    target <- tok register
    tok $ char '='
    invocant <- tok register
    char '.'
    identifier <- register
    arguments <- between (tok lparen) rparen $ sepBy (tok argument) (tok $ char ',')
    let pos = [ x | Pos x <- arguments]
        named = [x | (Named k v) <- arguments, x <- [k,v]]
    return $ Call target identifier (Capture invocant pos named)
call2 = do
    target <- tok register
    tok $ char '='
    responder <- tok register
    char '.'
    identifier <- register
    tok $ char '('
    tok $ char '|'
    capture <- register
    tok $ char ')'
    return $ Call2 target responder identifier capture

argument = do
        char ':'
        key <- tok register
        value <- between (tok $ char '(') (tok $ char ')') (tok register)
        return $ Named key value
    <|> do 
        arg <- tok register
        return $ Pos arg

terminator :: Parser ()
terminator = opt_ws >> (((tok $ char ';') >> return ()) <|> eof)
top = do 
    opt_ws
    stmts <- tok $ endBy stmt terminator
    eof
    return $ stmts

type RegMap = Map.Map [Char] Int
type LabelsMap = Map.Map [Char] Int

resolveReg r regs = Map.findWithDefault (error $ "undeclared register: $"++r) r regs 
resolveLabel l labels = Map.findWithDefault (error $ "undeclared label: "++l) l labels

toBytecode :: Stmt -> RegMap -> LabelsMap -> [Int]

toBytecode stmt regs labels = case stmt of
    Call target identifier (Capture invocant positional named) ->
        let reg r = resolveReg r regs
            args x = [length x] ++ map reg x
            in [1,reg target,reg invocant,reg identifier] ++ args positional ++ args named

    Call2 target responder identifier capture ->
        map (\r -> resolveReg r regs) [target,responder,identifier,capture]

    Goto label -> [3, resolveLabel label labels]

    Br value iftrue iffalse ->
        [4,resolveReg value regs,resolveLabel iftrue labels,resolveLabel iffalse labels]

    Labeled label stmt ->
        error "labels should be striped before being passed to toBytecode"

    Decl reg value -> []

isReg (Decl _ None) = True
isReg _ = False

countRegister stmts = length $ filter isReg stmts

addRegister :: RegMap -> Stmt -> RegMap
addRegister regs stmt = case stmt of 
    Decl reg None  -> regs
    Decl reg value -> Map.insert reg ((Map.size regs)+4)  regs
    _ -> regs


addFreeRegister :: RegMap -> Stmt -> RegMap
addFreeRegister regs stmt = case stmt of
    Decl reg None -> Map.insert reg ((Map.size regs)+4)  regs
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
    Labeled label stmt -> bytecodeLength stmt
    Call2 _ _ _ _ -> 5
    Decl _ _ -> 0

addLabel (labels,count) (Labeled label stmt) = (Map.insert label (count) labels,count+bytecodeLength stmt)
addLabel (labels,count) stmt = (labels,count+bytecodeLength stmt)

mapLabels :: [Stmt] -> LabelsMap
mapLabels stmts = fst $ foldl addLabel (Map.empty,0) stmts

emit :: [Stmt] -> RegMap -> LabelsMap -> [Int]
emit stmts regMap labelsMap = concatMap (\op -> toBytecode op regMap labelsMap) stmts ++ [0]

joinStr sep list = foldl (\a b -> a ++ sep ++ b) (head list) (tail list)

dumpConstantToC :: Value -> [Char]
dumpConstantToC value = case value of
    StringConstant str ->
        "SMOP__NATIVE__idconst_createn(\"" ++ str ++"\"," ++ (show $ length str) ++ "),"
    IntegerConstant int -> "SMOP__NATIVE__int_create(" ++ show int ++ "),"
    None -> ""
    Var name -> "SMOP_REFERENCE(interpreter," ++ name ++ "),"

dumpConstantsToC stmts = "(SMOP__Object*[]) {" ++
    concat [dumpConstantToC c | Decl reg c <- stmts] ++ "NULL}"

stripLabels (Labeled label stmt) = stripLabels stmt
stripLabels stmt = stmt

dumpToC unstriped_stmts =
    let labelsMap = mapLabels unstriped_stmts
        stmts = map stripLabels unstriped_stmts
        regMap    = mapRegisters stmts
        freeRegs  = countRegister stmts
        bytecode  = emit stmts regMap labelsMap
        constants = dumpConstantsToC stmts
        in "SMOP__Mold_create(" ++ show freeRegs ++ "," ++ constants ++ ","
        ++ show (length bytecode) ++ ",(int[]) {"
        ++ (joinStr "," $ map show bytecode)
        ++ "})"

main = do
    hFlush stdout
    line <- getContents
    case (parse top "" line) of 
        Left err      -> error  $ show err
        Right stmts -> do 
            --print stmts
            putStrLn $ dumpToC stmts
