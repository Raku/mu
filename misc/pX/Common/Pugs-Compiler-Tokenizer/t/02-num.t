
use Test::More tests => 4;

use_ok( 'Pugs::Grammar::Num' );

{
    my $match = Pugs::Grammar::Num->parse( q(0) );
    is( "$match", q(0), 'zero' );
}

{
    my $match = Pugs::Grammar::Num->parse( q(1) );
    is( "$match", q(1), 'num 1' );
}

{
    my $match = Pugs::Grammar::Num->parse( q(123) );
    is( "$match", q(123), 'num 123' );
}

