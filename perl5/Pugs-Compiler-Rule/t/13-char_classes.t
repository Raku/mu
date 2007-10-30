
use Test::More tests => 22;
use Data::Dumper;
$Data::Dumper::Indent = 1;

use_ok( 'Pugs::Compiler::Token' );
use_ok( 'Pugs::Compiler::Rule' );
no warnings qw( once );

use Pugs::Runtime::Match; # overload doesn't work without this ???

{
    #print "Ast: ", Dumper( Pugs::Grammar::Rule->rule( '[<before <.alpha>>|<before <.digit>>].' )->() );

    #print "Ast: ", Dumper( Pugs::Grammar::Rule->rule( '<+alpha+digit>+' )->() );

    my $rule = Pugs::Compiler::Token->compile( '<+alpha+digit>+' );
    my $match = $rule->match( "x3" );
    is( "$match", "x3", '<+alpha+digit>' );
}

{
    {
    package test;
    use Pugs::Compiler::Regex;
    use base 'Pugs::Grammar::Base';
    Pugs::Compiler::Regex->reinstall( rule => '<+[xy]>+' );
    }
    my $match = test->rule( 'yx' );
    is( "$match", "yx", '<+[user char class]>' );
}

{
    my $rule = Pugs::Compiler::Regex->compile( '<+[A]+xdigit>+' );
    #print $rule->perl5;
    my $match = $rule->match( '3A' );
    is( "$match", "3A", '<+[user char class]+class>' );
}

{
    my $rule = Pugs::Compiler::Regex->compile( '^<+[A]+xdigit>' );
    #print $rule->perl5;
    my $match = $rule->match( '3' );
    is( "$match", "3", '<+[user char class]+class>' );
}

{
    {
    package test;
    use Pugs::Compiler::Regex;
    use base 'Pugs::Grammar::Base';
    Pugs::Compiler::Regex->reinstall( rule => '<-[ \x20 ]>+' );
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
    is( "".$rule->match( "b" ), "",  'b' );
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

{
    my $rule = Pugs::Compiler::Regex->compile( '^<+[a..z]-[bx]>$' );
    is( "".$rule->match( "3" ), "",  '3 ^<+[a..z]-[bx]>$ regex' );
    is( "".$rule->match( "a" ), "a", 'a' );
    is( "".$rule->match( "b" ), "",  'b' );
}

{
    my $rule = Pugs::Compiler::Regex->compile( '^<+[3]+[a..z]-[bx]>$' );
    is( "".$rule->match( "3" ), "3", '3 ^<+[a..z]-[bx]>$ regex' );
    is( "".$rule->match( "a" ), "a", 'a' );
    is( "".$rule->match( "b" ), "",  'b' );
}
