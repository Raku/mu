
class (Typeable a) => RuleClass a where
    rule_iType :: a -> Type
    rule_iType = const $ mkType "Regex"
    rule_fetch :: a -> Eval VRule
    rule_store :: a -> VRule -> Eval ()
    rule_match :: a -> VStr -> Eval (MatchResult Val)
