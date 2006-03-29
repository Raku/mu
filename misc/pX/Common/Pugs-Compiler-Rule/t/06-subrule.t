use Test::More tests => 20;
use Data::Dumper;
$Data::Dumper::Indent = 1;

use_ok( 'Pugs::Compiler::Rule' );

{
    my $rule = Pugs::Compiler::Rule->compile('\w');
    #print $rule->perl5;
    my $rule2 = eval $rule->perl5;
    my $match = $rule2->match("abc");
    is( "$match",'a',"perl5 returns eval'able code");
}

{
    local *Test123::rule1 = Pugs::Compiler::Rule->compile('\w')->code();
    local *Test123::rule2 = Pugs::Compiler::Rule->compile('(<rule1>)*')->code();
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
    local *Test123::rule1 = Pugs::Compiler::Rule->compile('\w')->code();
    local *Test123::rule2 = Pugs::Compiler::Rule->compile('<rule1>*')->code();
    my $match = Test123->rule2("abc");
    is($match,'abc',"Matched...");
    is(ref($match->{rule1}),"ARRAY",'$<rule1> is an array...');
    is( $match->{rule1}[0],"a","Capture 1...");
    is( $match->{rule1}[1],"b","Capture 2...");
    is( $match->{rule1}[2],"c","Capture 3...");
    is( $match->{rule1}[3],undef,"No more captures");
}

{
    local *Test123::rule1 = Pugs::Compiler::Rule->compile('\w')->code();
    local *Test123::rule2 = Pugs::Compiler::Rule->compile('<rule1><rule1>')->code();
    my $match = Test123->rule2("abc");
    is($match,'ab',"Matched...");
    is(ref($match->{rule1}),"ARRAY",'$<rule1> is an array...');
    is( $match->{rule1}[0],"a","Capture 1...");
    is( $match->{rule1}[1],"b","Capture 2...");
    is( $match->{rule1}[3],undef,"No more captures");
}

{
SKIP: { 
    skip "backtracking into subrules disabled", 1;
    # backtracking into subrules
    my $rule1 = Pugs::Compiler::Rule->compile('\w+');
    my $rule2 = Pugs::Compiler::Rule->compile('a<$rule1>z');

    print $rule1->perl5;
    print $rule2->perl5;

    my $match = $rule2->match("abcz");
    is($match,'abcz',"Matched...");
    is(ref($match->{rule1}),"ARRAY",'$<rule1> is an array...');
    is( $match->{rule1}[0][0],"a","Capture 1...");
    is( $match->{rule1}[0][1],"b","Capture 2...");
    is( $match->{rule1}[0][3],undef,"No more captures");
}
}
