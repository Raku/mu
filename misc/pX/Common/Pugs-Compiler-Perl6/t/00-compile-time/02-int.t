use Test::More tests => 1;
use Data::Dumper;

use Pugs::Emitter::Perl6::Perl5::Value;
use Pugs::Emitter::Perl6::Perl5::Expression;

sub node {
    eval 'use Pugs::Emitter::Perl6::Perl5::' . $_[0];
    ( 'Pugs::Emitter::Perl6::Perl5::' . $_[0] )->new( { name => $_[1] } );
}

TODO: {
    local $TODO = 'no tests yet';
    ok( 0, 'nada' );
};
__END__

sub exp {
    return node( 'Int', 
        if ( exists
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

# 4 types of compile-time bool: 
# unboxed,boxed x eager,lazy

{
    my $b = node( 'Bool', 1 );
    #print Dumper( $b );
    is( "$b", 1, 'emit bool' );
    is( "" . $b->not, 0, 'emit bool.not' );
    is( "" . $b->str, "'1'", 'emit bool.str' );
    is( "" . $b->WHAT, "'Bool'", 'emit bool.WHAT' );
    is( "" . $b->int, "1", 'emit bool.int' );
    is( "" . $b->num, "1", 'emit bool.num' );
    is( "" . $b->true, "1", 'emit bool.true' );

    # ==
    my $i = node( 'Int', 42 );
    is( "" . $b->_61__61_( $i ) , "0", 'emit bool.==(num)' );
    is( "" . $b->_61__61_( $i )->not , "1", 'emit bool.==(num).not' );

}
