
use Test::More tests => 3;

use_ok( 'Pugs::Compiler::Rule' );
#no warnings qw( once );

{
    my $rule = Pugs::Compiler::Regex->compile( '(.)$0' );
    my $match = $rule->match( "xxy" );
    is( "$match", "xx", 'match $0' );
}

{
    my $rule = Pugs::Compiler::Regex->compile( '(.)(.)$1' );
    my $match = $rule->match( "xyyz" );
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
