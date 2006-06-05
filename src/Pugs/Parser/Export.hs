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

exportSym :: Scope -> String -> Val -> RuleParser Exp
exportSym scope ('&':subname) ref = do
    Val (VList subs) <- unsafeEvalExp $ Syn "@{}" [Val ref]
    exps <- forM subs $ \val -> do
        let name    = '&':subname
            mkMulti = case val of
                VCode sub | isMulti sub -> ('&':)
                _                       -> id
            mkExp   = Syn ":=" [Var name, Val val]
            mkSym   = Sym scope (mkMulti name) mkExp
        doExport scope mkSym
    return $ case scope of
        SMy -> Pad SState (foldl unionPads (mkPad []) [ pad | Pad SMy pad _ <- exps ]) emptyExp
        _   -> emptyExp 
exportSym scope subname@(sig:_) ref | isWordAlpha sig = do
    exportSym scope ('&':subname) ref
exportSym _ _ _ = fail "notyet"
--exportSym' scope name sym = other vars...

doExport :: Scope -> Exp -> RuleParser Exp
doExport SGlobal sym = do
    unsafeEvalExp sym
    return emptyExp
doExport SMy sym = do
    lexDiff <- unsafeEvalLexDiff $ sym
    return $ Pad SMy lexDiff emptyExp
doExport _ _ = fail "notyet" -- XXX writeme. but do they all make sense at all?
