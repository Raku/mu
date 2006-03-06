{-# OPTIONS_GHC -fglasgow-exts -funbox-strict-fields -fno-full-laziness -fno-cse #-}

module Pugs.Parser.Doc (
    ruleDocBlock,
) where
import Pugs.Internals
import Pugs.AST
import Pugs.Lexer
import Pugs.Rule
import Pugs.Parser.Types

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

{-|
Match \'@=cut@\', followed by a newline (see 'ruleWhiteSpaceLine').

The \'@=@\' must be the first character of the line ('ruleDocIntroducer').
-}
ruleDocCut :: RuleParser ()
ruleDocCut = (<?> "Doc cut") $ do
    ruleDocIntroducer
    string "cut"
    ruleWhiteSpaceLine
    return ()

ruleDocBody :: RuleParser ()
ruleDocBody = (try ruleDocCut) <|> eof <|> do
    many $ satisfy  (/= '\n')
    many1 newline -- XXX - paragraph mode
    ruleDocBody
    return ()

ruleDocBlock :: RuleParser Exp
ruleDocBlock = verbatimRule "Doc block" $ do
    isEnd <- try $ do
        ruleDocIntroducer
        section <- do
            c <- wordAlpha
            cs <- many $ satisfy (not . isSpace)
            return (c:cs)
        param <- option "" $ do
            satisfy isSpace
            -- XXX: drop trailing spaces?
            many $ satisfy (/= '\n')
        return (section == "begin" && param == "END")
    choice [ eof, do { many1 newline; return () } ]
    if isEnd
        then do
            many anyChar
            return emptyExp
        else do
            ruleDocBody
            whiteSpace
	    return emptyExp
