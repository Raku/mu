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
    return ()

opt_ws = option () ws

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


hint = do
    symbol "RI" 
    parenthesized $ do
        reg <- tok $ register
        symbol ","
        ri <- constant_string
        return $ [Hint RI reg (StringConstant ri)]
         
stmt = do 
    l <- option [] (try label)
    body <- choice $ [goto,br,hint,noop,declaration,action]
    return $ l ++ body

action = do
   lvalue <- tok $ register
   symbol "="
   val <- value
   option [Assign lvalue val] $ (symbol ".") >> (call val lvalue)


declaration = do
   symbol "my" 
   reg <- tok $ register
   (defaultValue,action) <- option (None,[]) $ symbol "=" >> expression reg
   return $ [Decl reg defaultValue] ++ action

expression lvalue = expression_constant_first lvalue <|> expression_reg_first lvalue

expression_constant_first lvalue = do
    c <- constant
    option (c,[]) $ do
        symbol "."
        reg <- declare_implicitly c
        action <- call reg lvalue
        return (None,action)


expression_reg_first lvalue = do
    r <- tok $ register
    option (None,[Assign lvalue r]) $ symbol "." >>(call r lvalue >>= \x -> return (None,x))


call invocant lvalue = do
    identifier <- value
    arguments <- parenthesized $ sepBy (tok argument) (symbol ",")
    let pos = [ x | Pos x <- arguments]
        named = [x | (Named k v) <- arguments, x <- [k,v]]
        action = [Call (lvalue :: Register) identifier (Capture (invocant :: Register) pos named)]
    return action

    




{- TODO - port over
call2 = do
    target <- value
    symbol "="
    responder <- value
    char '.'
    identifier <- value
    capture <- parenthesized $ symbol "|" >> value
    return [Call2 target responder identifier capture]
-}

constant_string = between (char '"') (char '"') quotedChars >>= return . concat
    where
      quotedChars = many $
        do
        c <- noneOf "\"\\"
        return [c]
        <|> do 
            char '\\'
            c <- anyChar
            return ['\\',c]

constant = choice 
      [ do
          (char '¢') <|> (char '?')
          name <- identifier
          return $ Var name
      , do
        digits <- many1 digit 
        return $ IntegerConstant $ read digits
      , do
        content <- constant_string
        return $ StringConstant content
      , submold
      ]

-- implicit_decl :: GenParser Char ImplicitDecls [Char]
--implicit_decl

implicit_decl = do
    c <- constant
    declare_implicitly c

declare_implicitly c = do
    decls <- getState
    case (Map.lookup c decls) of
        Just c -> return c
        Nothing ->
            do 
                let new = "___implicit_register___"++(show $ Map.size decls)
                updateState $ Map.insert c new
                return $ new

value = tok $ choice [register,implicit_decl]


noop = string "noop" >> return []

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

implicitDecls = concat . map (\(constant,reg) -> [Hint Constant reg constant,Decl reg constant]) . Map.toList

parseM0ld code = 
    case (runParser top (Map.empty :: ImplicitDecls) "" code) of 
        Left err      -> error  $ show err
        Right (stmts,constants) -> (implicitDecls constants) ++ stmts
