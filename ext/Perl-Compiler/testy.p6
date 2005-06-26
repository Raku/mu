use Perl::Compiler::PIL;
use Perl::Compiler::PIL::Util;
use Perl::Compiler::CodeGen::NameGen;
use Perl::Compiler::CodeGen::Perl5_Str;

# push @array, 4;
my $tree = ::Perl::Compiler::PIL::PILStmt.new(
            value => ::Perl::Compiler::PIL::PILApp.new(
                code => ::Perl::Compiler::PIL::PILExp.new(
                    value => ::Perl::Compiler::PIL::PILVar.new(
                        value => '&push',
                    ),
                ),
                args => [
                    ::Perl::Compiler::PIL::PILExp.new(
                        ::Perl::Compiler::PIL::PILVar.new(
                            value => '@array',
                        ),
                    ),
                    ::Perl::Compiler::PIL::PILLit.new(
                        ::Perl::Compiler::PIL::PILVal.new(
                            value => 4,
                        ),
                    ),
                ],
            ),
           );

my $gen = ::Perl::Compiler::CodeGen::Perl5_Str.new;
say $tree.ref;
say $gen.generate($tree);
