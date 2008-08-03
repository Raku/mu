import Text.ParserCombinators.Parsec
import System.IO
type Label = [Char]
type Register = [Char]
data Value = Var [Char] | IntegerConstant Integer | None
    deriving Show
data Capture = Register [Register] [Register]
    deriving Show
data Stmt = Decl Register Value | Goto Label | Br Register Label Label | Call Register Capture
    deriving Show

data Mold = Mold [Stmt]
    deriving Show

identifier = do
    first <- choice [alphaNum,char '_']
    rest <- many1 $ choice [alphaNum,char '_',digit]
    return $ [first] ++ rest
ws = many1 $ (oneOf "\t\n " >> return () ) <|> (char '#' >> many1(noneOf "\n") >> newline >> return () )
opt_ws = option [()] ws

register =  char '$' >> identifier

stmt = choice [decl]

value = do
        char '$'
        name <- identifier
        return $ Var name
    <|> do
        digits <- many1 digit 
        return $ IntegerConstant $ read digits
decl = do 
    string "my"
    ws
    x <- register
    value <- option None $ opt_ws >> char '=' >> opt_ws >> value
    return (Decl x value)


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
