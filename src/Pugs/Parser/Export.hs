{-# OPTIONS_GHC -fglasgow-exts #-}

{-|
    Symbol export.
-}

module Pugs.Parser.Export (
    exportSym,
) where
import Pugs.Internals
import Pugs.AST

import Pugs.Parser.Types
import Pugs.Parser.Unsafe
import Pugs.Lexer (isWordAlpha)

exportSym :: Scope -> String -> Val -> RuleParser ()
exportSym scope name@('&':_) ref = do
    pad <- unsafeEvalLexDiff $ _Sym scope name mempty (Val ref) Noop
    addBlockPad pad
exportSym scope subname@(sig:_) ref | isWordAlpha sig = do
    exportSym scope ('&':subname) ref
exportSym _ _ _ = fail "Non-Code exports does not work yet"
--exportSym' scope name sym = other vars...
