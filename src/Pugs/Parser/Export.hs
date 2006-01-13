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
    --trace ("exporting sub: " ++ subname) $ return ()
    (Val (VList subs)) <- unsafeEvalExp $ Syn "@{}" [Val ref]
    env <- getRuleEnv
    let newPkg = envPackage env
    exps <- forM subs (\(VCode sub) -> do
        let qName = '&':newPkg ++ "::" ++ subname
        let mkMulti = if isMulti sub then ('&':) else id
        {- notes on the various takes at fixing export to work with multi subs.
         - replacing VCode's subName field doesn't seem to have much effect.
         - the variations seem to be what to put on the lhs of the := expression
           (unqualified/qualified to old package/qualified to new package) and
           what name to give the symbol.
         - it's helpful when debugging this to uncomment the trace prints in
           genSym and genMultiSym (Pugs.AST).
         - the exporter is getting big, and it doesn't even support variables
           yet. we need to refactor this out to another file+more functions.
        -}
        --let mkExp = Syn ":=" [Var (if isMulti sub then qName else name), Syn "sub" [Val $ VCode sub{ subName=qName }]]
        --let mkExp = Syn ":=" [Var (if isMulti sub then qName else name), Syn "sub" [Val $ VCode sub]]
        --trace ((if (isMulti sub) then qName else name) ++ ":=") $ return ()
        let mkExp = Syn ":=" [Var (if (isMulti sub) then qName else '&':subname), Syn "sub" [Val $ VCode sub]]
        --let mkExp = Syn ":=" [Var name, Syn "sub" [Val $ VCode sub]]
        --let mkSym = Sym scope (mkMulti qName) mkExp
        let mkSym = Sym scope (if (isMulti sub) then (mkMulti qName) else '&':subname) mkExp
        --trace ("export: " ++ (show mkSym)) $ return ()
        doExport scope mkSym mkExp)
    return $ foldl mergeStmts Noop exps
--exportSym' scope name sym = other vars...

doExport :: Scope -> Exp -> Exp -> RuleParser Exp
doExport SGlobal sym _ = do
    unsafeEvalExp sym
    return emptyExp
doExport SMy sym exp = do
    lexDiff <- unsafeEvalLexDiff $ sym
    return $ Pad SMy lexDiff $ exp
doExport _ _ _ = fail "notyet" -- XXX writeme. but do they all make sense at all?
