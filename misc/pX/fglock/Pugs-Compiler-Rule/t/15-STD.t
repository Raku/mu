
use Test::More tests => 6;
use Data::Dumper;
$Data::Dumper::Indent = 1;

use_ok( 'Pugs::Compiler::Regex' );
no warnings qw( once );

use base 'Pugs::Grammar::Base';

sub dump_match {  print "Match: ",         $_[0]->perl      }
sub dump_ast   {  print "Ast: ",   Dumper( $_[0]->{ast}  )  }
sub dump_rule  {  print "Rule: ",          $_[0]->{perl5}   }

{
    my $rule = Pugs::Compiler::Regex->compile( 'x {*}' );
    my $match = $rule->match( "x" );
    is( "$match", "x", 'whatever' );
}

{
    my $rule = Pugs::Compiler::Regex->compile( '<before x> x' );
    my $match = $rule->match( "x" );
    is( "$match", "x", 'before-rule' );
}

{
    my $rule = Pugs::Compiler::Regex->compile( '<before: x> x' );
    my $match = $rule->match( "x" );
    is( "$match", "x", 'before-string' );
}

{
    my $rule = Pugs::Compiler::Regex->compile( '<before("x")> x' );
    #dump_ast( $rule );
    #dump_rule;
    my $match = $rule->match( "x" );
    is( "$match", "x", 'before-as-function' );
}

{
    my $rule = Pugs::Compiler::Regex->compile( ' x {*} y' );
    #dump_ast( $rule );
    #dump_rule;
    my $match = $rule->match( "xy" );
    is( "$match", "xy", 'ignore Whatever' );
}

