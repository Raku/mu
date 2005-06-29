use Perl::Compiler::PIL;
use Perl::Compiler::PIL::Util;
use Perl::Compiler::CodeGen::NameGen;
use Perl::Compiler::CodeGen::Perl5_Str;

# push @array, 4;

my $pad = ::Perl::Compiler::PIL::Util::Pad.new(
            names => [ '@array', '&push' ],
);

my $tree = ::Perl::Compiler::PIL::PILStmt.new(
            value => ::Perl::Compiler::PIL::PILApp.new(
                code => ::Perl::Compiler::PIL::PILExp.new(
                    value => ::Perl::Compiler::PIL::PILVar.new(
                        value => '&push',
                        pad => $pad,
                    ),
                ),
                args => [
                    ::Perl::Compiler::PIL::PILExp.new(
                        value => ::Perl::Compiler::PIL::PILVar.new(
                            value => '@array',
                            pad => $pad,
                        ),
                    ),
                    ::Perl::Compiler::PIL::PILLit.new(
                        value => ::Perl::Compiler::PIL::PILVal.new(
                            value => 4,
                        ),
                    ),
                ],
            ),
           );

my $gen = ::Perl::Compiler::CodeGen::Perl5_Str.new;
say $gen.generate($tree);
