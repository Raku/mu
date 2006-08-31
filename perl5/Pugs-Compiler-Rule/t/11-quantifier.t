
use Test::More tests => 13;
use Data::Dumper;

use_ok( 'Pugs::Compiler::Regex' );
use_ok( 'Pugs::Compiler::Token' );
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

{
    # capture count restarts in alternation
    my $rule = Pugs::Compiler::Regex->compile( '(\d+) | ([_|\w]+)' );
    my $match = $rule->match( "xyzw" );
    #print Dumper( $match->data );
    #print Dumper( $match->[0]->data );
    is( "$match->[0]", "xyzw", '(\d+) | ([_|\w]+)' );
    is( $match->[0](), "xyzw", '(\d+) | ([_|\w]+)' );
}


# -- Token


{
    # capture count restarts in alternation
    my $rule = Pugs::Compiler::Token->compile( '(\d+) | ([_|\w]+)' );
    my $match = $rule->match( "xyzw" );
    #print Dumper( $match->data );
    #print Dumper( $match->[0]->data );
    is( "$match->[0]", "xyzw", '(\d+) | ([_|\w]+)' );
    is( $match->[0](), "xyzw", '(\d+) | ([_|\w]+)' );
}

{
    # non-capture quantified match failure does not propagate
    my $rule = Pugs::Compiler::Token->compile( ' [ <!before k > . ]+ { return $/() } ' );
    my $match = $rule->match( "12x31kwww" );
    #print Dumper( $match->data );
    is( "$match", "12x31", '[\d]+ as scalar' );
    is( "" . $match->() , "12x31", '[\d]+ as scalar' );
}
