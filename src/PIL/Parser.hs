{-# OPTIONS_GHC -fglasgow-exts #-}

module PIL.Parser (parse) where
import PIL.Val
import PIL.Syn
import PIL.Pad
import PIL.Monads
import PIL.Internals
import Pugs.Rule hiding (parse)
import Control.Monad.Identity

type RuleParser a = GenParser Char RuleState a
type RuleState = ()
type Parse = IO

-- Parsing needs to handle BEGIN and such.
parse :: String -> Parse Exp
parse src = case ( runParser ruleProgram () "-" src ) of
    Left err    -> fail (show err)
    Right exp   -> return exp

ruleProgram :: RuleParser Exp
ruleProgram = ruleExp `fin` eof

ruleExp :: RuleParser Exp
ruleExp = choice
    [ ruleApp
    , ruleLit
    ]

ruleApp :: RuleParser Exp
ruleApp = do
    s <- ruleSymBare
    spaces
    e <- ruleExp
    return $ App (Var s) e

ruleSymBare :: RuleParser Sym
ruleSymBare = do
    x   <- letter
    xs  <- many alphaNum
    mkSym ('&':x:xs)

ruleLit :: RuleParser Exp
ruleLit = do
    i <- many1 digit
    return . Lit . Single . Int . read $ i

