
use Perl::Compiler::CodeGen::NameGen;

class Perl::Compiler::CodeGen::Perl5_Str
    does Perl::Compiler::CodeGen {

    my $INS = 'Perl6::Internals';

    method generate (Perl::Compiler::PIL::PIL $tree is rw) {
        my $ng = ::Perl::Compiler::CodeGen::NameGen.new({ "\$P_$_" });
        ./gen($tree, $ng);
    }

    method gen (Perl::Compiler::PIL::PIL $tree is rw, PIL::Compiler::CodeGen::NameGen $ng is rw) {
        given $tree {
            when ::Perl::Compiler::PIL::PILNil    { '' }

            when ::Perl::Compiler::PIL::PILNoop   { ';' }
            
            when ::Perl::Compiler::PIL::PILLit    { ./gen(.value, $ng) }

            when ::Perl::Compiler::PIL::PILExp    { ./gen(.value, $ng) }

            when ::Perl::Compiler::PIL::PILPos    { ./gen(.value, $ng) }

            when ::Perl::Compiler::PIL::PILStmt   { ./gen(.value, $ng.fork('expr')) ~ $ng.r('expr') }

            when ::Perl::Compiler::PIL::PILThunk  { 
                $ng.ret("$INS\::p5_make_thunk( sub () \{ { ./gen(.value) } } )");  ''
            }

            when ::Perl::Compiler::PIL::PILCode   { 
                $ng.ret("$INS\::p5_make_code( sub \{ { ./gen(.statments) } } )");  ''
            }

            when ::Perl::Compiler::PIL::PILVal    { 
                $ng.ret("$INS\::p5_make_val( { .value } )");  ''
            }

            when ::Perl::Compiler::PIL::PILVar    { $ng.ret(.value); '' }    # handwave

            when ::Perl::Compiler::PIL::PILStmts  { ./gen(.head, $ng.fork) ~ '; ' ~ ./gen(.tail, $ng.fork) }

            when ::Perl::Compiler::PIL::PILApp    {
                my $str = join ' ',
                    ./gen(.code, $ng.fork('code')),
                    map { ./gen($^arg, $^gen) } zip([.args], [map { $ng.fork("arg$_") } 0 ..^ .args]);
                $ng.ret(
                    $ng.r('code') ~ '->CALL(' 
                        ~ join(', ', map { $ng.r("arg$_") } 0 ..^ .args) ~ ')'
                );
                $str;
            }

            when ::Perl::Compiler::PIL::PILAssign {
                my $str = ./gen(.right, $ng.fork('right')) ~ ./gen(.left, $ng.fork('left'));
                $ng.ret(
                    $ng.r('left') ~ "->ASSIGN( $ng.r('right') )"
                );
                $str;
            }
            
            when ::Perl::Compiler::PIL::PILBind   {
                my $str = ./gen(.right, $ng.fork('right')) ~ ./gen(.left, $ng.fork('left'));
                $ng.ret(
                    $ng.r('left') ~ "->BIND( $ng.r('right') )"
                );
                $str;
            }

            die "Unknown PIL node type: $tree.ref()";
        }
    }
}

# vim: ft=perl6 :
