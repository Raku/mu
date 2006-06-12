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
exportSym scope ('&':subname) ref = do
    rv <- unsafeEvalExp $ Syn "," [Syn "@{}" [Val ref]]
    case rv of
        Val (VList subs) -> do
            exps <- forM subs $ \val -> do
                let name    = '&':subname
                    mkMulti = case val of
                        VCode sub | isMulti sub -> ('&':)
                        _                       -> id
                    mkExp   = Syn ":=" [Var name, Val val]
                    mkSym   = Sym scope (mkMulti name) mkExp
                doExport scope mkSym
            case scope of
                SMy -> addBlockPad SState 
                    (foldl unionPads (mkPad []) [ pad | Pad SMy pad _ <- exps ])
                _   -> return () 
        _ -> fail $ "Invalid export list: " ++ show rv
exportSym scope subname@(sig:_) ref | isWordAlpha sig = do
    exportSym scope ('&':subname) ref
exportSym _ _ _ = fail "Non-Code exports does not work yet"
--exportSym' scope name sym = other vars...

doExport :: Scope -> Exp -> RuleParser Exp
doExport SGlobal sym = do
    unsafeEvalExp sym
    return emptyExp
doExport SMy sym = do
    lexDiff <- unsafeEvalLexDiff $ sym
    return $ Pad SMy lexDiff emptyExp
doExport _ _ = fail "notyet" -- XXX writeme. but do they all make sense at all?
