
use Test::More tests => 3;

use_ok( 'Pugs::Compiler::Rule' );
use Pugs::Grammar::Base;
#no warnings qw( once );

{
    my $rule = Pugs::Compiler::Regex->compile( '(.)$0' );
    #print "Source: ", $rule->perl;
    my $match = $rule->match( "xxy" );
    is( "$match", "xx", 'match $0' );
}

{
    my $rule = Pugs::Compiler::Regex->compile( '(.)(.)$1' );
    my $match = $rule->match( "xyyz" );
    #print "Source: ", $rule->perl;
    #print "Match: ", $match->perl;
    is( "$match", "xyy", 'match $1' );
}

=for later
{
    my $rule = Pugs::Compiler::Regex->compile( '((.)$0[0])' );
    my $match = $rule->match( "xx" );
    is( $match?1:0, 1, 'booleanify - unnamed rules are objects' );
    is( "$match", "xyz", 'match $0[0]' );
}
=cut
