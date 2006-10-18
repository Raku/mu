use Test::More tests => 1;
use Data::Dumper;

use Pugs::Emitter::Perl6::Perl5::Value;
use Pugs::Emitter::Perl6::Perl5::Native;
use Pugs::Emitter::Perl6::Perl5::Expression;

# 'infix:<+>' eq infix_58__60__43__62_

sub emit {
    return Pugs::Emitter::Perl6::Perl5::node->node( 'int', $_[0]{int} )
        if ( exists $_[0]{int} );
    return emit( $_[0]{exp1} )->infix_58__60__43__62_( emit( $_[0]{exp1} ) )
        if (  exists $_[0]{fixity}
           && $_[0]{fixity} eq 'infix'
           && $_[0]{op1} eq '+' );
    die 'not implemented: ', Dumper( $_[0] );
}

# 1+1
my $ast = {
      'exp1' => {
        'int' => '1',
        'pos' => 1
      },
      'exp2' => {
        'int' => '1',
        'pos' => 3
      },
      'fixity' => 'infix',
      'op1' => '+'
};

my $i = emit( $ast );
is( "" . $i, "2", "AST evaluates correctly" );



