module M0ld.Parser (parseM0ld) where
import Text.ParserCombinators.Parsec hiding (label)
import qualified Data.Map as Map
import M0ld.AST

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
          (char '¢') <|> (char '?')
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
type ImplicitDecls = Map.Map Value [Char]

implicitDecls = map (\(constant,reg) -> Decl reg constant) . Map.toList

parseM0ld code = 
    case (runParser top (Map.empty :: ImplicitDecls) "" code) of 
        Left err      -> error  $ show err
        Right (stmts,constants) -> (implicitDecls constants) ++ stmts
