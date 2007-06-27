
use Test::More tests => 5;

use_ok( 'Pugs::Compiler::Rule' );
use Pugs::Grammar::Base;
#no warnings qw( once );
use Data::Dumper;

{
    my $rule = Pugs::Compiler::Regex->compile( '(.)$0' );
    #print "Source: ", $rule->perl;
    my $match = $rule->match( "xxy" );
    is( "$match", "xx", 'match $0' );
    $match = $rule->match('xyy');
    is $match->str, 'yy';
}

{
    my $rule = Pugs::Compiler::Regex->compile( '(.)(.)$1' );
    my $match = $rule->match( "xyyz" );
    #print "Ast: ", Dumper( Pugs::Grammar::Rule->rule( '(.)(.)$1' )->() );
    #print "Source: ", $rule->perl;
    #print "Match: ", $match->perl;
    is( "$match", "xyy", 'match $1' );
    $match = $rule->match('xyxx');
    is "$match", 'yxx';
}

=for later

{
    my $rule = Pugs::Compiler::Regex->compile( '((.)$0[0])' );
    my $match = $rule->match( "xx" );
    #warn "Ast: ", Dumper( Pugs::Grammar::Rule->rule( '((.)$0[0])' )->() );
    #warn "Source: ", $rule->perl;
    #warn "Match: ", $match->perl;
    is( $match?1:0, 1, 'booleanify - unnamed rules are objects' );
    is( "$match", "xyz", 'match $0[0]' );
}

=cut
