
use Test::More tests => 4;

use_ok( 'Pugs::Grammar::Term' );

{
    my $match = Pugs::Grammar::Term->parse( q("abc") );
    is( "$match", q("abc"), 'double quoted str' );
}

{
    my $match = Pugs::Grammar::Term->parse( q($abc) );
    is( "$match", q($abc), 'scalar var' );
}

{
    my $match = Pugs::Grammar::Term->parse( q(10) );
    is( "$match", q(10), 'num' );
}
