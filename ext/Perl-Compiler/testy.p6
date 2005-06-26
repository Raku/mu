use Perl::Compiler::PIL;
use Perl::Compiler::PIL::Util;
use Perl::Compiler::CodeGen::NameGen;
use Perl::Compiler::CodeGen::Perl5_Str;

my $noop = ::Perl::Compiler::PIL::PILNoop.new;
my $gen = ::Perl::Compiler::CodeGen::Perl5_Str.new;
say $gen.generate($noop);
