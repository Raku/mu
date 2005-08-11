use perl5:Code::Perl::Expr (":easy");

use Perl::Compiler::CodeGen::NameGen;

class Perl::Compiler::CodeGen::Perl5_Str
    does Perl::Compiler::CodeGen {

    my $INS = 'Perl6::Internals';

    method generate (Perl::Compiler::PIL::PIL $tree is rw) {
        my $ng = ::Perl::Compiler::CodeGen::NameGen.new(template => { "\$P_$_" });
        say "$?SELF / $?SELF.ref()";
        ./gen($tree, $ng);
    }

    method gen (Perl::Compiler::PIL::PIL $tree is rw, PIL::Compiler::CodeGen::NameGen $ng is rw) {
        my $ret = do given $tree {
            say "Processing $tree / $tree.ref()";
            
            when ::Perl::Compiler::PIL::PILNil    { "; # Nil\n" }

            when ::Perl::Compiler::PIL::PILNoop   { "; # Noop\n" }
            
            when ::Perl::Compiler::PIL::PILLit    { ./gen(.value, $ng) }

            when ::Perl::Compiler::PIL::PILExp    { ./gen(.value, $ng) }

            when ::Perl::Compiler::PIL::PILPos    { ./gen(.value, $ng) }

            when ::Perl::Compiler::PIL::PILStmt   { ./gen(.value, $ng.fork('expr')) ~ $ng.r('expr') }

            when ::Perl::Compiler::PIL::PILThunk  { 
                $ng.ret("$INS\::p5_make_thunk( sub () \{ { ./gen(.value) } } )");  ''
            }

            when ::Perl::Compiler::PIL::PILCode   { 
                my $inner = ./gen(.statements);
                $ng.ret("$INS\::p5_make_code( sub \{ { 
                    (join "\n", map { 
                        "my " ~ ./pad_var($_)
                    } $tree.pads)
                    ~ $inner
                } } )");  ''
            }

            when ::Perl::Compiler::PIL::PILVal    {
                my sub box (String $class, $value) {
                    callm(string($class), "new", $value);
                }
                $ng.ret(do given .value {
                    when Str { box("P5::PIL::Run::Str" => string($_)) }
                    when Num { box("P5::PIL::Run::Number" => number($_)) }
                    when Bool { &?OUTER::BLOCK(+$_) }
                    when undef { box("P5::PIL::Run::Undef" => perl("undef")) }
                    when List { box("P5::PIL::Run::List" => list(map { &OUTER::BLOCK($_) } *$_)) }
                    when Error { box("P5::PIL::Run::Error", string($_.first), list(@{ $_.second })) }
                    when Junc { die "no junctions yet"; box("P5::PIL::Run::Junction", ...) }
                    default { die "a value of type $_.ref cannot appear in PIL. Your compiler must be sick." }
                }.perl); ''
            }

            when ::Perl::Compiler::PIL::PILVar    {
                # XXX shouldn't need $tree.pad.id ; .pad.id should do ($tree is topic)
                my $pad = $tree.pad;
                $ng.ret(./pad_var($pad) ~ "->\{'{ $tree.value }'}"); ''
            }

            when ::Perl::Compiler::PIL::PILStmts  { 
                ./gen(.head, $ng.fork) ~ '; ' ~ ./gen(.tail, $ng.fork);
            }

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
        };

        say "RETVAL = $ret";
        return $ret;
    }

    method pad_var(Perl::Compiler::PIL::Util::Pad $pad) {
        "\$PAD_" ~ $pad.id;
    }
}

# vim: ft=perl6 :
