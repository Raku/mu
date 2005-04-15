{-# OPTIONS_GHC -fglasgow-exts -fth #-}

module Pugs.IMC.Compile where

import Rule
import Lexer
import Text.PrettyPrint hiding (char)
import IMC.AST
import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax

nested = nest 4

class Pretty a where
    format :: a -> Doc

instance Show (Term a) where
    show x = show $ format x

instance Pretty (Term a) where
    format (TStr str) = ptext $ show str
    format (TOp1 op term) = ptext op <+> format term
    format (TSub name stmts) =
        text ".sub" <+> text name
        $+$ nested (format stmts)
        $+$ text ".sub"

instance Pretty [Term Statement] where
    format = vcat . map format

eol = do { many1 newline ; return () } <|> eof

sub :: Parser (Term Sub)
sub = do
    symbol ".sub"
    name <- identifier
    -- eol
    stmts <- many stmt
    symbol ".end"
    eol
    return $ TSub name stmts

stmt :: Parser (Term Statement)
stmt = do
    op <- identifier
    operand <- stringConstant
    eol
    return $ TOp1 op operand

stringConstant :: Parser (Term String)
stringConstant = do
    char '"'
    str <- many $ string "\\\"" <|> many1 (noneOf "^\"\n")
    char '"'
    return $ TStr $ concat str


-- imcCompile :: Term a -> ExpQ
imcX :: String -> ExpQ
imcX str = do 
    -- let bar = pprint exp
    -- runIO $ putStrLn $ bar
    let foo = imcParse sub str
    imcCompile foo
{-
    do
    str <- runQ exp
    let bar = ppr str
    runIO $ putStrLn $ show bar
    let foo = imcParse sub prog
    imcCompile foo
-}

imcParse :: Parser (Term a) -> String -> Term a
imcParse p str = case ( runParser p () "-" str ) of
    Left err    -> error $ show err
    Right t     -> t

parsed = imcParse sub ".sub main\nprint \"123\"\nprint \"456\"\n.end"
mrr = imcCompile parsed

prog = ".sub main\nprint \"123\"\nprint \"456\"\n.end"

zzz :: IO ()
zzz = do
    x <- runQ mrr
    putStrLn $ show $ ppr x

-- run :: Term a -> IO (Term a)
-- run program = $( compile [| program |] )
foo program = $( compile [| program |] )


{-
.sub main
    print "Hello"
.end
-}

-- imcRun $ imcParse sub ".sub main\nprint \"123\"\nprint \"456\"\n.end"

