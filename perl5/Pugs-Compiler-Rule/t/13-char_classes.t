
use Test::More tests => 4;
use Data::Dumper;
$Data::Dumper::Indent = 1;

use_ok( 'Pugs::Compiler::Token' );
use_ok( 'Pugs::Compiler::Rule' );
no warnings qw( once );

use Pugs::Runtime::Match; # overload doesn't work without this ???

{
    #print "Ast: ", Dumper( Pugs::Grammar::Rule->rule( '[<before <?alpha>>|<before <?digit>>].' )->() );

    #print "Ast: ", Dumper( Pugs::Grammar::Rule->rule( '<+alpha+digit>+' )->() );

    my $rule = Pugs::Compiler::Token->compile( '<+alpha+digit>+' );
    my $match = $rule->match( "x3" );
    is( "$match", "x3", '<+alpha+digit>' );
}

SKIP: {
    skip 'Infinite Loop', 1;
    
    package test;
    use Pugs::Compiler::Regex;
    use base 'Pugs::Grammar::Base';

    Pugs::Compiler::Regex->install( rule => '<subrule>.' );
    Pugs::Compiler::Regex->install( subrule => '<-[ ]>+' );
    my $match = test->rule( 'xx' );
}

