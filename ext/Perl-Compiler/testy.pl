use Perl::Compiler::PIL;
use Perl::Compiler::PIL::Util;
use Perl::Compiler::CodeGen::NameGen;
use Perl::Compiler::CodeGen::Perl5_Str;

# push @array, 4;

my $pad = ::Perl::Compiler::PIL::Util::Pad.new(
            names => [ '@array', '&push' ],
);

my $stmt = ::Perl::Compiler::PIL::PILStmt.new(
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

my $block = ::Perl::Compiler::PIL::PILCode.new(
                codetype => ::Perl::Compiler::PIL::Util::ConcreteType.new(
                    name => 'Block',
                ),
                signature => ::Perl::Compiler::PIL::Util::Signature.new(
                    params => [],
                ),
                pads => [ $pad ],
                statements => ::Perl::Compiler::PIL::PILStmts.new(
                    head => $stmt,
                    tail => ::Perl::Compiler::PIL::PILNil.new(),
                ),
            );

my $gen = ::Perl::Compiler::CodeGen::Perl5_Str.new;
say $gen.generate($block);
