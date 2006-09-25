use Test::More tests => 8;
use Data::Dumper;

use Pugs::Emitter::Perl6::Perl5::Value;
use Pugs::Emitter::Perl6::Perl5::Expression;

sub node {
    eval 'use Pugs::Emitter::Perl6::Perl5::' . $_[0];
    ( 'Pugs::Emitter::Perl6::Perl5::' . $_[0] )->new( { name => $_[1] } );
}

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
    # TODO - evaluate ' 1 == 42 ' at compile-time
    my $i = node( 'Int', 42 );
    is( "" . $b->_61__61_( $i ) , "1 == 42", 'emit bool.==(num)' );

}
