{-# OPTIONS_GHC -fglasgow-exts #-}
module Pugs.Parser.Literal where

import Pugs.Internals
import Pugs.AST
import Pugs.Lexer
import Pugs.Rule

import Pugs.Parser.Types
import Pugs.Parser.Operator

{-|
Match one of the \'yada-yada-yada\' placeholder expressions (@...@, @???@ or
@!!!@), returning a call to @&fail@, @&warn@ or @&die@ respectively.
-}
yadaLiteral :: RuleParser Exp
yadaLiteral = expRule $ do
    sym  <- choice . map symbol $ words " ... ??? !!! "
    return $ App (_Var $ doYada sym) Nothing [Val $ VStr (sym ++ " - not yet implemented")]
    where
    doYada "..." = "&fail_" -- XXX rename to fail() eventually
    doYada "???" = "&warn"
    doYada "!!!" = "&die"
    doYada _ = error "Bad yada symbol"

{-|
Match the given literal string (as a lexeme), returning the second argument in
a 'Pugs.AST.Internals.Val' expression.

Used by 'ruleLit' for @NaN@ and @Inf@.
-}
namedLiteral :: String -- Literal string to match
             -> Val    -- Value to return
             -> RuleParser Exp
namedLiteral n v = do { symbol n; return $ Val v }

ruleCommaOrSemicolon :: RuleParser ()
ruleCommaOrSemicolon = do
    lexeme (oneOf ",;")
    return ()

ruleTwigil :: RuleParser String
ruleTwigil = option "" . choice . map string $ words " ^ * ? . ! + ; "

ruleMatchPos :: RuleParser String
ruleMatchPos = do
    sigil   <- oneOf "$@%"
    digits  <- many1 digit
    return $ (sigil:digits)

ruleMatchNamed :: RuleParser String
ruleMatchNamed = do
    sigil   <- oneOf "$@%"
    twigil  <- char '<'
    name    <- many (do { char '\\'; anyChar } <|> satisfy (/= '>'))
    char '>'
    return $ (sigil:twigil:name) ++ ">"

ruleDot :: RuleParser ()
ruleDot = verbatimRule "dot" $ do
    try (char '.' >> notFollowedBy (char '.')) <|> ruleLongDot
    optional $ oneOf "*+?"

ruleLongDot :: RuleParser ()
ruleLongDot = do
    try (char '\\' >> notFollowedBy (char '('))
    whiteSpace
    char '.'
    return ()

-- zero-width, non-consuming word boundary assertion (\b)
ruleWordBoundary :: RuleParser ()
ruleWordBoundary = do
    cls <- getPrevCharClass
    look $ if (cls == SpaceClass) then (/=) else (==)
    return ()
    where
    look op = lookAhead (satisfy (\c -> SpaceClass `op` charClassOf c))

