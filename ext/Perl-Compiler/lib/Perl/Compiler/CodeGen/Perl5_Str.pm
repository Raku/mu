
use Perl::Compiler::CodeGen::NameGen;

class Perl::Compiler::CodeGen::Perl5_Str
    does Perl::Compiler::CodeGen {
    method generate (Perl::Compiler::PIL::PIL $tree) {
        my $ng = ::Perl::Compiler::CodeGen::NameGen.new({ "\$P_$_" });
        ./gen($tree, $ng);
    }

    method gen (Perl::Compiler::PIL::PIL $tree, PIL::Compiler::CodeGen::NameGen $ng) {
        given $tree {
            when ::Perl::Compiler::PIL::PILNil    { '' }
            when ::Perl::Compiler::PIL::PILNoop   { ';' }
            when ::Perl::Compiler::PIL::PILLit    { ./gen(.value, $ng.fork('RET')) }
            when ::Perl::Compiler::PIL::PILExp    { ./gen(.value, $ng.fork('RET')) }
            when ::Perl::Compiler::PIL::PILPos    { ./gen(.value, $ng.fork('RET')) }
            when ::Perl::Compiler::PIL::PILStmt   { ./gen(.value, $ng.fork('RET')) }
            when ::Perl::Compiler::PIL::PILThunk  { $ng.ret('sub () { ' ~ ./gen(.value) ~ ' }'); '' }
            when ::Perl::Compiler::PIL::PILCode   { 'sub { ' ~ ./gen(.statments) ~ ' }' }
            when ::Perl::Compiler::PIL::PILVal    { $ng.ret(.value); '' }
            when ::Perl::Compiler::PIL::PILVar    { $ng.ret(.value); '' }    # handwave
            when ::Perl::Compiler::PIL::PILStmts  { ./gen(.head, $ng.fork) ~ '; ' ~ ./gen(.tail, $ng.fork) }
            when ::Perl::Compiler::PIL::PILApp    {
                # XXX this idiom needs refactoring
                my $str = join ' ',
                    ./gen(.code, $ng.fork('code')),
                    map { ./gen($^arg, $^gen) } .args ¥ map { $ng.fork("arg$_") } 0..^.args;
                $ng.ret(
                    '&{' ~ $ng.r('code') ~ '}'
                    ~ '(' ~ join(', ', map { $ng.r("arg$_") } 0..^.args) ~ ')'
                );
                $str;
            }
            when ::Perl::Compiler::PIL::PILAssign {
                my $str = join ' ',
                    ./gen(.right, $ng.fork('right')),
                    map { ./gen($^arg, $^gen) } .args ¥ map { $ng.fork("left$_") } 0..^.args;
                $ng.ret(
                    '(' ~ join(', ', map { $ng.r("left$_") } 0..^.lefts) ~ ') = ' ~ $ng.right
                );
                $str;
            }
            when ::Perl::Compiler::PIL::PILBind   {
                my $str = join ' ',
                    ./gen(.right, $ng.fork('right')),
                    map { ./gen($^arg, $^gen) } .args ¥ map { $ng.fork("left$_") } 0..^.args;
                $ng.ret(
                    '(' ~ join(', ', map { $ng.r("left$_") } 0..^.lefts) ~ ') XXX:= ' ~ $ng.right
                );
                $str;
            }
        }
    }
}

# vim: ft=perl6 :
