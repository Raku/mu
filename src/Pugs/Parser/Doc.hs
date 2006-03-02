{-# OPTIONS_GHC -fglasgow-exts -funbox-strict-fields -fno-full-laziness -fno-cse #-}

module Pugs.Parser.Doc (
    ruleDocIntroducer,
    ruleDocBody,
) where
import Pugs.Internals
import Pugs.AST
import Pugs.Types
import Pugs.Lexer
import Pugs.Rule
import Pugs.Rule.Expr

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
