grammar Grammar::Perl6;
use Pugs::Grammar::Rule;
rule rule_decl {
    rule <p6ws> <ident> <p6ws>? \{ <Pugs::Grammar::Rule::rule> \}
        { return { rule_decl => $() ,} }
}
push @statements, \&rule_decl;
