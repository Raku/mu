
use Test::More tests => 3;

use_ok( 'Pugs::Compiler::Rule' );

{
    my $rule = Pugs::Compiler::Rule->compile( '((.).)(.)' );
    my $match = $rule->match( "xyzw" );
    is( eval { "$match" }, "xyz", 'stringify 1' );
    is( eval { "$match->[0]" }, "xy", 'stringify 2' );
    is( eval { "$match->[0][0]" }, "x", 'stringify 3' );
}

