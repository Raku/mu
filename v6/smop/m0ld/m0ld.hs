import Text.ParserCombinators.Parsec
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
    first <- choice [alphaNum,char '_']
    rest <- many1 $ choice [alphaNum,char '_',digit]
    return $ [first] ++ rest
ws = many1 $ (oneOf "\t\n " >> return () ) <|> (char '#' >> many1(noneOf "\n") >> newline >> return () )
opt_ws = option [()] ws

register =  char '$' >> identifier

stmt = choice [call,decl]

value = do
        char '$'
        name <- identifier
        return $ Var name
    <|> do
        digits <- many1 digit 
        return $ IntegerConstant $ read digits
    <|> do
        char '"'
        content <- many $ noneOf "\"\\" <|> (char '\\' >> anyChar)
        char '"'
        return $ StringConstant content
decl = do 
    string "my"
    ws
    x <- register
    value <- option None $ opt_ws >> char '=' >> opt_ws >> value
    return (Decl x value)
call = do
    target <- register
    opt_ws
    char '='
    opt_ws
    invocant <- register
    opt_ws
    char '.'
    identifier <- register
    char '('
    opt_ws
    many argument
    opt_ws
    char ')'
    return $ Call target identifier (Capture invocant [] [])
argument =
        do
        char ':'
        key <- register
        char '('
        value <- register
        char ')'
        return $ Named key value
    <|> (register >>= return . Pos)

terminator :: Parser ()
terminator = opt_ws >> ((char ';' >> opt_ws >> return ()) <|> eof)
top = do 
    opt_ws
    stmts <- endBy1 stmt terminator
    opt_ws
    eof
    return $ Mold stmts
main = do
    hFlush stdout
    line <- getContents
    putStr $ "input:" ++ line
    case (parse top "" line) of 
        Left err      -> print err
        Right opcodes -> print opcodes
