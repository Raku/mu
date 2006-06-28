use Test::More tests => 30;
use Data::Dumper;
$Data::Dumper::Indent = 1;
use strict;
use warnings;
no warnings 'once';

use_ok( 'Pugs::Compiler::Regex' );

{
    my $rule = Pugs::Compiler::Regex->compile('\w');
    my $rule2 = eval $rule->perl5;
    warn $@ if $@;
    is( ref($rule2), 'Pugs::Compiler::Regex', "a Regex object");
    my $match = $rule2->match("abc");
    is( "$match",'a',"perl5 returns eval'able code");
}

{
    local *Test123::rule1 = Pugs::Compiler::Regex->compile('\w')->code();
    local *Test123::rule2 = Pugs::Compiler::Regex->compile('(<rule1>)*')->code();
    my $match = Test123->rule2("abc");
    #print Dumper( $$match );
    is($match,'abc',"Matched...");
    is( $match->[0][0],"a","Capture 1...");
    is( $match->[0][1],"b","Capture 2...");
    is( $match->[0][2],"c","Capture 3...");
    is( $match->[0][3],undef,"No more captures");
    is( $match->[1],undef,"No more captures");
}

{
    local *Test123::rule1 = Pugs::Compiler::Regex->compile('\w')->code();
    local *Test123::rule2 = Pugs::Compiler::Regex->compile('<rule1>*')->code();
    my $match = Test123->rule2("abc");
    is($match,'abc',"Matched...");
    is(ref($match->{rule1}),"ARRAY",'$<rule1> is an array...');
    is( $match->{rule1}[0],"a","Capture 1...");
    is( $match->{rule1}[1],"b","Capture 2...");
    is( $match->{rule1}[2],"c","Capture 3...");
    is( $match->{rule1}[3],undef,"No more captures");
}

{
    local *Test123::rule1 = Pugs::Compiler::Regex->compile('\w')->code();
    local *Test123::rule2 = Pugs::Compiler::Regex->compile('<rule1><rule1>')->code();
    my $match = Test123->rule2("abc");
    is($match,'ab',"Matched...");
    is(ref($match->{rule1}),"ARRAY",'$<rule1> is an array...');
    is( $match->{rule1}[0],"a","Capture 1...");
    is( $match->{rule1}[1],"b","Capture 2...");
    is( $match->{rule1}[3],undef,"No more captures");
}

{
    # backtracking into subrules
    my $rule1 = Pugs::Compiler::Regex->compile('(\w)+');
    my $rule2 = Pugs::Compiler::Regex->compile('a<$rule1>z');

    #print $rule1->perl5;
    #print $rule2->perl5;

    my $match = $rule2->match("abcz");
    is( "$match",'abcz',"backtracking subrule matched");

    #print Dumper($match[0]);
    is(ref($match->[0]),"ARRAY",'array...');
    is( $match->[0][0],"b","Capture 1...");
    is( $match->[0][1],"c","Capture 2...");
    is( $match->{rule1}[0][3],undef,"No more captures");
}

{
    # before
    my $rule = Pugs::Compiler::Regex->compile('(a)<before z>');
    #print $rule->perl5;
    
    my $match = $rule->match("az");
    is( "$match",'a',"before matched");

    no warnings qw( uninitialized );
    
    $match = $rule->match("a");
    is( "$match",'',"before didn't match");
    
    $match = $rule->match("ab");
    is( "$match",'',"before didn't match");
}

SKIP:
{
    skip "named parameters don't parse correctly", 1;
    my $subrule = Pugs::Compiler::Regex->compile(' .* $^a{to} ');
    #print $subrule->perl5;

    {
        package Test;
        use base 'Pugs::Grammar::Base';
        *subrule = $subrule->code;
    }
    
    my $rule = Pugs::Compiler::Regex->compile(' \[ <Test.subrule(to=>"]")> ');
    my $match = $rule->match("[abc]");
    #print Dumper $match;
    #print $match->(), "\n";
    is( "$match",'[abc]',"subrule+param matched");
}

SKIP: {
    skip "failing optional quantifier - subrule + param\n", 1;

    my $subrule = Pugs::Compiler::Regex->compile(' .*? $^a ');
    #print $subrule->perl5;

    {
        package Test;
        use base 'Pugs::Grammar::Base';
        *subrule2 = $subrule->code;
    }
    
    my $rule = Pugs::Compiler::Regex->compile(' \[ <Test.subrule2("]")> ');
    my $match = $rule->match("[abc]");
    #print Dumper $match;
    #print $match->(), "\n";
    is( "$match",'[abc]',"subrule+param matched");
}

