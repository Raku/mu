import Text.ParserCombinators.Parsec
import qualified Data.Map
import System.IO
type Label = [Char]
type Register = [Char]
data Value = Var [Char] | IntegerConstant Integer | StringConstant [Char] | None
    deriving Show
data Capture = Capture Register [Register] [Register]
    deriving Show
data Stmt = Decl Register Value | Goto Label | Br Register Label Label | Call Register Register Capture
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

stmt = choice [call,decl]

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
        return $ StringConstant content
      ]
      where
      quotedChar = many $ noneOf "\"\\" <|> (char '\\' >> anyChar)

decl = do 
    string "my"
    ws
    x <- tok register
    value <- option None $ (tok $ char '=') >> value
    return (Decl x value)

call = do
    target <- tok register
    tok $ char '='
    invocant <- tok register
    char '.'
    identifier <- register
    between (tok lparen) rparen $ tok $ many argument
    tok $ many argument
    return $ Call target identifier (Capture invocant [] [])

argument = do
        char ':'
        key <- register
        value <- between (char '(') (char ')') register
        return $ Named key value
    <|> (register >>= return . Pos)

terminator :: Parser ()
terminator = opt_ws >> (((tok $ char ';') >> return ()) <|> eof)
top = do 
    opt_ws
    stmts <- tok $ endBy1 stmt terminator
    eof
    return $ stmts

type RegMap = Data.Map.Map [Char] Int

toBytecode :: Stmt -> RegMap -> [Int]
toBytecode _ regs = []

isReg (Decl _ None) = True
isReg _ = False

countRegister stmts = length $ filter isReg stmts

addRegister :: RegMap -> Stmt -> RegMap
addRegister regs (Decl reg None) = regs
addRegister regs (Decl reg value) = Data.Map.insert reg ((Data.Map.size regs)+4)  regs
addRegister regs _ = regs

registerMap :: [Stmt] -> RegMap
registerMap stmts = foldl addRegister Data.Map.empty stmts

emit :: [Stmt] -> RegMap -> [Int]
emit stmts regMap = foldl (++) [] $ map (\op -> toBytecode op regMap) stmts

main = do
    hFlush stdout
    line <- getContents
    putStr $ "input:" ++ line
    case (parse top "" line) of 
        Left err      -> print err
        Right stmts -> do 
            print stmts
            let regMap = registerMap stmts
            let freeRegs = countRegister stmts
            putStr "uninitalised registers: "
            print freeRegs 
            putStr "register name->int: "
            print regMap
            print $ emit stmts regMap
