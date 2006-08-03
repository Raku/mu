
use Test::More tests => 6;
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
    #print Dumper( $match->data );
    #print Dumper( $match->[0]->data );
    is( "$match->[0]", "xyzw", '(.*)' );
}

{
    my $rule = Pugs::Compiler::Regex->compile( '([_|\w]+)' );
    my $match = $rule->match( "xyzw" );
    #print Dumper( $match->data );
    #print Dumper( $match->[0]->data );
    is( "$match->[0]", "xyzw", '([_|\w]+)' );
    is( $match->[0](), "xyzw", '([_|\w]+)' );
}
