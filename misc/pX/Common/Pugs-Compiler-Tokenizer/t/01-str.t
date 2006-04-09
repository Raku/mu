
use Test::More tests => 3;

use_ok( 'Pugs::Grammar::Str' );

{
    my $match = Pugs::Grammar::Str->parse( q("abc") );
    is( "$match", q("abc"), 'double quoted str' );
}

{
    my $match = Pugs::Grammar::Str->parse( q('abc') );
    is( "$match", q('abc'), 'single quoted str' );
}

