
use Test::More tests => 12;
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

TODO: 
{
    local $TODO = 'Char class with whitespace';
    {
    package test;
    use Pugs::Compiler::Regex;
    use base 'Pugs::Grammar::Base';

    Pugs::Compiler::Regex->install( rule => '<subrule>.' );
    Pugs::Compiler::Regex->install( subrule => '<-[ \x20 ]>+' );
    }
    my $match = test->rule( 'xx' );
    is( "$match", "xx", 'not-ws in quantified subrule' );
}

{
    # from pugs stdrules.t
    my $rule = Pugs::Compiler::Token->compile( '^<+alpha>$' );
    my $match = $rule->match( "x" );
    is( "$match", "x", '^<+alpha>$ token' );
}

{
    # from pugs stdrules.t
    my $rule = Pugs::Compiler::Regex->compile( '^<+alpha>$' );
    my $match = $rule->match( "x" );
    is( "$match", "x", '^<+alpha>$ regex' );
}

{
    # from pugs stdrules.t
    my $rule = Pugs::Compiler::Token->compile( '^<-alpha>$' );
    my $match = $rule->match( "3" );
    is( "$match", "3", '^<-alpha>$ token' );
}

{
    # from pugs stdrules.t
    my $rule = Pugs::Compiler::Regex->compile( '^<-alpha>$' );
    my $match = $rule->match( "3" );
    is( "$match", "3", '^<-alpha>$ regex' );
}

# ---

{
    # from pugs stdrules.t
    my $rule = Pugs::Compiler::Token->compile( '^<+[ax]>$' );
    my $match = $rule->match( "x" );
    is( "$match", "x", '^<+[ax]>$ token' );
}

{
    # from pugs stdrules.t
    my $rule = Pugs::Compiler::Regex->compile( '^<+[ax]>$' );
    my $match = $rule->match( "x" );
    is( "$match", "x", '^<+[ax]>$ regex' );
}

{
    # from pugs stdrules.t
    my $rule = Pugs::Compiler::Token->compile( '^<-[ax]>$' );
    my $match = $rule->match( "3" );
    is( "$match", "3", '^<-[ax]>$ token' );
}

{
    # from pugs stdrules.t
    my $rule = Pugs::Compiler::Regex->compile( '^<-[ax]>$' );
    my $match = $rule->match( "3" );
    is( "$match", "3", '^<-[ax]>$ regex' );
}
