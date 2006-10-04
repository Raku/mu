use Test::More tests => 9;
use Data::Dumper;

use Pugs::Emitter::Perl6::Perl5::Value;
use Pugs::Emitter::Perl6::Perl5::Expression;

# 4 types of compile-time bool: 
# unboxed,boxed x eager,lazy

{
    my $b = Pugs::Emitter::Perl6::Perl5::node->node( 'Bool', 1 );
    #print Dumper( $b );
    is( "$b", 1, 'emit bool' );
    is( "" . $b->not, 0, 'emit bool.not' );
    is( "" . $b->str, "'1'", 'emit bool.str' );
    is( "" . $b->WHAT, "'Bool'", 'emit bool.WHAT' );
    is( "" . $b->int, "1", 'emit bool.int' );
    is( "" . $b->num, "1", 'emit bool.num' );
    is( "" . $b->true, "1", 'emit bool.true' );

    # 'infix:<==>' eq infix_58__60__61__61__62_

    my $i = Pugs::Emitter::Perl6::Perl5::node->node( 'Int', 42 );
    is( "" . $b->infix_58__60__61__61__62_( $i ) , "0", 'emit bool.==(num)' );
    is( "" . $b->infix_58__60__61__61__62_( $i )->not , "1", 'emit bool.==(num).not' );

}
