
use Test::More tests => 6;

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

{
    my $match = Pugs::Grammar::Num->parse( q(Inf) );
    is( "$match", q(Inf), 'num Inf' );
}

{
    my $match = Pugs::Grammar::Num->parse( q(NaN) );
    is( "$match", q(NaN), 'num NaN' );
}
