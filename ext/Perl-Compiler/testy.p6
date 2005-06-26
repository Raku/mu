use Perl::Compiler::PIL;
use Perl::Compiler::PIL::Util;
use Perl::Compiler::CodeGen::NameGen;
use Perl::Compiler::CodeGen::Perl5_Str;

my $tree = ::Perl::Compiler::PIL::PILStmt.new(
            value => ::Perl::Compiler::PIL::PILLit.new(
                value => ::Perl::Compiler::PIL::PILVal.new(
                    value => 42,
                ),
            ),
           );

my $gen = ::Perl::Compiler::CodeGen::Perl5_Str.new;
say $tree.ref;
say $gen.generate($tree);
