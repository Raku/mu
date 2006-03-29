use Test::More tests => 24;
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
=for docs - see S05
    *   An interpolated hash matches the longest possible key of the hash as
        a literal, or fails if no key matches. (A "" key will match
        anywhere, provided no longer key matches.)
    *   If the corresponding value of the hash element is a closure, it is executed.
    *   If it is a string or rule object, it is executed as a subrule.
    *   If it has the value 1, nothing special happens beyond the match.
    *   Any other value causes the match to fail.
=cut
    my $match;
    my $v = 0;
    my %test = (
        if => 2,        # fail (not '1') or string '2' ???
        iff => 1,       # match (longer than 'if')
        until => Pugs::Compiler::Rule->compile(".*aa"),  
                        # subrule - match "until(.*aa)"
        use => sub { $v = 1 },   
                        # closure - print "use()"
    );   
    $rule1 = Pugs::Compiler::Rule->compile('%test 123');
    
    $match = $rule1->match("iff123");
    is($match,'iff123',"Matched hash{iff}");

    $match = $rule1->match("if123");
    is($match,'',"fail hash{if} - value != 1");

    is($v,0,"closure not called yet");
    $match = $rule1->match("use");
    is($v,1,"closure was called hash{use}");

    $match = $rule1->match("until bbaa");
    is($match,'',"subrule hash{until}");

}
