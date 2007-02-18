{-# OPTIONS_GHC -fglasgow-exts -funbox-strict-fields -fno-full-laziness -fno-cse #-}

module Pugs.Parser.Doc (
    ruleDocBlock,
) where
import Pugs.Internals
import Pugs.AST
import Pugs.Lexer
import Pugs.Rule
import Pugs.Parser.Types
import Pugs.Parser.Unsafe

-- Inline Documentation ----------------------------------------

{-|
Assert that we're at the beginning of a line, but consume no input (and produce
no result).

Used by 'ruleDocIntroducer', because POD-style regions must have their \'@=@\'
at the beginning of a line.
-}
ruleBeginOfLine :: RuleParser ()
ruleBeginOfLine = do
    pos <- getPosition
    when (sourceColumn pos /= 1) $ fail ""
    return ()

{-|
Match a single \'@=@\', but only if it occurs as the first character of a line.
-}
ruleDocIntroducer :: RuleParser Char
ruleDocIntroducer = (<?> "Doc intro") $ do
    ruleBeginOfLine
    char '='

ruleDocBody :: DocHead -> RuleParser [String]
ruleDocBody docHead = (eof >> return []) <|> do
    line    <- ruleDocLine
    case dropTrailingSpaces line of
        "=cut"  -> return []
        "=end"  -> return []
        ('=':'e':'n':'d':' ':sec) | docHead /= Misc, sec == headSection docHead -> return []
        ""  | For{} <- docHead  -> return []
        _       -> do
            lines   <- ruleDocBody docHead
            return (line:lines)

ruleDocLine :: RuleParser String
ruleDocLine = many (satisfy (/= '\n'))
    `finallyM` (eof <|> (newline >> return ()))

dropTrailingSpaces :: String -> String
dropTrailingSpaces = reverse .  dropWhile isSpace . reverse

type Section = String
data DocHead
    = Begin { headSection :: Section }
    | For { headSection :: Section, headText :: String }
    | Cut
    | Misc
    deriving (Eq)

ruleDocBlock :: RuleParser Exp
ruleDocBlock = verbatimRule "Doc block" $ do
    docHead <- try $ do
        ruleDocIntroducer
        introducer <- do
            c   <- wordAlpha
            cs  <- many $ satisfy (not . isSpace)
            return (c:cs)
        section <- option "" $ do
            skipMany1 (char ' ')
            ruleDocLine
        return $ case introducer of
            "begin" | not (null section)    -> Begin (dropTrailingSpaces section)
            "for"   | not (null section)    -> uncurry For (break isSpace section)
            "cut"   | null section          -> Cut
            _                               -> Misc
    case docHead of
        Cut -> do
            fail "=cut does not start a POD block; please remove this line"
            return emptyExp  -- "=cut" does not start a block (unspecced but useful)
        Misc -> do
            ruleDocBody Misc
            whiteSpace
            return emptyExp
        Begin "END" -> do
            setInput ""
            return emptyExp
        _ -> do
            let section = headSection docHead 
            rv <- do
                lns <- ruleDocBody docHead
                let lns' | For { headText = (_:txt) } <- docHead = txt:lns
                         | otherwise = lns
                    linesVal    = map VStr lns'
                    linesStr    = unlines lns'
                    linesList   = VList (length linesVal `seq` linesVal)
                unsafeEvalExp $ Stmts
                    (App (_Var "&push") (Just $ _Var ("@=" ++ section)) [Val (VStr linesStr)])
                    $ Stmts 
                        (App (_Var "&push") (Just $ _Var ("$=" ++ section)) [Val linesList])
                        (App (_Var "&push") (Just $ Syn "{}" [_Var "%=POD", Val (VStr section)]) [Val linesList])
            whiteSpace
            return (rv `seq` emptyExp)
