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
    rv <- unsafeEvalExp $ Syn "," [App (_Var "&values") (Just (Val ref)) []]
    case rv of
        Val (VList subs) -> do
            pads <- forM (filter defined subs) $ \val -> do
                let name    = '&':subname
                    sym     = _Sym scope name mempty (Val val) Noop
                unsafeEvalLexDiff sym
            case scope of
                SMy -> addBlockPad (foldl' unionPads (mkPad []) pads)
                _   -> return () 
        _ -> fail $ "Invalid export list: " ++ show rv
exportSym scope subname@(sig:_) ref | isWordAlpha sig = do
    exportSym scope ('&':subname) ref
exportSym _ _ _ = fail "Non-Code exports does not work yet"
--exportSym' scope name sym = other vars...
