
use Test::More tests => 2;
use Data::Dumper;

use_ok( 'Pugs::Compiler::Regex' );
use_ok( 'Pugs::Grammar::Base' );

{
    my $rule = Pugs::Compiler::Regex->compile( '.*' );
    my $match = $rule->match( "xyzw" );
    is( "$match", "xyzw", '.*' );
}

{
    my $rule = Pugs::Compiler::Regex->compile( '(.*)' );
    my $match = $rule->match( "xyzw" );
    is( "$match[0]", "xyzw", '(.*)' );
}

