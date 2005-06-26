
class Perl::Compiler::CodeGen::Perl5 
    does Perl::Compiler::CodeGen {
    method generate (Perl::Compiler::PIL::PIL $tree) {
        say $tree;
        ./gen($tree);
    }

    method gen (Perl::Compiler::PIL::PIL $tree) {
        $_ = $tree;
            when Perl::Compiler::PIL::PILNil    { '' }
            when Perl::Compiler::PIL::PILNoop   { ';' }
            when Perl::Compiler::PIL::PILLit    { ./gen(.value) }
            when Perl::Compiler::PIL::PILExp    { ./gen(.value) }
            when Perl::Compiler::PIL::PILPos    { ./gen(.value) }
            when Perl::Compiler::PIL::PILStmt   { ./gen(.value) }
            when Perl::Compiler::PIL::PILThunk  { 'sub () { ' ~ ./gen(.value) ~ ' }' }
            when Perl::Compiler::PIL::PILCode   { 'sub { ' ~ ./gen(.statments) ~ ' }' }
            when Perl::Compiler::PIL::PILVal    { .value }
            when Perl::Compiler::PIL::PILVar    { .value }
            when Perl::Compiler::PIL::PILStmts  { ./gen(.head) ~ '; ' ~ ./gen(.tail) }
            when Perl::Compiler::PIL::PILApp    { 
                '&{' ~ ./gen(.code) ~ '}(' ~ .args.map:{ ./gen($_) }.join(', ') ~ ')';
            }
            when Perl::Compiler::PIL::PILAssign {
                '(' ~ .lefts.map:{ ./gen($_) }.join(', ') ~ ') = ' ~ ./gen(.right);
            }
            when Perl::Compiler::PIL::PILBind   {
                '(' ~ .lefts.map:{ ./gen($_) }.join(', ') ~ ') XXX:= ' ~ ./gen(.right);
            }
    }
}

# vim: ft=perl6 :
