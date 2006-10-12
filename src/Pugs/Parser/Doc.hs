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

ruleDocBody :: String -> RuleParser [String]
ruleDocBody section = (eof >> return []) <|> do
    line    <- many (satisfy (/= '\n'))
    newline -- XXX - paragraph mode
    case line of
        "=cut"  -> return []
        "=end"  -> return []
        ('=':'e':'n':'d':' ':sec) | sec == section -> return []
        _       -> do
            lines   <- ruleDocBody section
            return (line:lines)

ruleDocBlock :: RuleParser Exp
ruleDocBlock = verbatimRule "Doc block" $ do
    section <- try $ do
        ruleDocIntroducer
        introducer <- do
            c   <- wordAlpha
            cs  <- many $ satisfy (not . isSpace)
            return (c:cs)
        section <- option "" $ do
            char ' '
            -- XXX: drop trailing spaces?
            many $ satisfy (/= '\n')
        return (if introducer == "begin" then section else "")
    eof <|> skipMany1 newline
    if section == "END" then setInput "" >> return emptyExp else do
        rv <- case section of
            ""  -> ruleDocBody "" >> return Noop
            _   -> do
                lines <- ruleDocBody section
                let linesVal    = map VStr lines
                    linesList   = VList (length linesVal `seq` linesVal)
                unsafeEvalExp $ Stmts
                    (App (_Var "&push") (Just $ _Var ("@=" ++ section)) [Val (VStr (unlines lines))])
                    $ Stmts 
                        (App (_Var "&push") (Just $ _Var ("$=" ++ section)) [Val linesList])
                        (App (_Var "&push") (Just $ Syn "{}" [_Var "%=POD", Val (VStr section)]) [Val linesList])
        whiteSpace
        return (rv `seq` emptyExp)
