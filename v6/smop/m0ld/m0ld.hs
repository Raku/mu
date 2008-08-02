import Text.ParserCombinators.Parsec
import System.IO
type Label = [Char]
type Register = [Char]

data Stmt = RegisterDecl Register | ConstantDecl Register | Goto Label | Br Register Label Label
    deriving Show

data Mold = Mold [Stmt]
    deriving Show

ws = many1 space
opt_ws = many space
register = do 
    char '$'
    many1 alphaNum

stmt = opt_ws >> choice [register_decl,constant_decl]

register_decl = do 
    string "my"
    ws
    x <- register
    return (RegisterDecl x)

constant_decl = do
    string "constant"
    ws
    x <- register
    return (ConstantDecl x)

terminator :: Parser ()
terminator = opt_ws >> ((char ';' >> opt_ws >> return ()) <|> eof)
top = do 
    stmts <- endBy1 stmt terminator
    eof
    return $ Mold stmts
main = do
    hFlush stdout
    line <- getContents
    putStr $ "input:" ++ line
    case (parse top "" line) of 
        Left err      -> print err
        Right opcodes -> print opcodes
