
use Test::More tests => 3;
use Data::Dumper;

use_ok( 'Pugs::Runtime::Rule' );  # lrep-generated rule parser
use_ok( 'Pugs::Runtime::Rule2' ); # user rule parser
use_ok( 'Pugs::Grammar::Rule' );
use_ok( 'Pugs::Emitter::Rule::Perl5' );
use_ok( 'Pugs::Runtime::Match' );

{
    my $rule = Rule->new( '((.).)(.)' );
    my $match = $rule->match( "xyzw" );
    is( eval { "$match" }, "xyz", 'stringify 1' );
    is( eval { "$match->[0]" }, "xy", 'stringify 2' );
    is( eval { "$match->[0][0]" }, "x", 'stringify 3' );
}

