{-# OPTIONS_GHC -fglasgow-exts #-}

{-|
    Symbol export.
-}

module Pugs.Parser.Export (
    exportSym,
) where
import Pugs.Internals
import Pugs.Rule
import Pugs.AST

import Pugs.Parser.Types
import Pugs.Parser.Unsafe

exportSym :: Scope -> String -> Val -> RuleParser Exp
exportSym scope ('&':subname) ref = do
    (Val (VList subs)) <- unsafeEvalExp $ Syn "@{}" [Val ref]
    env <- getRuleEnv
    let newPkg = envPackage env
    exps <- forM subs $ \(VCode sub) -> do
        let name = ('&':subname)
            mkMulti = if isMulti sub then ('&':) else id
            mkExp = Syn ":=" [Var name, Syn "sub" [Val $ VCode sub]]
            mkSym = Sym scope (mkMulti name) mkExp
        doExport scope mkSym
    return $ case scope of
        SMy -> Pad SMy (foldl unionPads (mkPad []) [ pad | Pad SMy pad _ <- exps ]) emptyExp
        _   -> emptyExp 
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
