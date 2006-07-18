use Test::More tests => 8;
use Data::Dump::Streamer;
$Data::Dump::Streamer::Indent = 1;

use_ok( 'Pugs::Compiler::Rule' );

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
        if =>    2,        # fail (number, not '1')
        iff =>   1,        # match (longer than 'if')
        until => Pugs::Compiler::Regex->compile('(a.a)'),  
                           # subrule - match "until(aa)"
        use =>   sub { $v = 1 },   
                           # closure - print "use()"
    );   
    $rule1 = Pugs::Compiler::Regex->compile('%test 123');
    
    $match = $rule1->match("iff123");
    is($match,'iff123',"Matched hash{iff}");

    $match = $rule1->match("if123");
    is($match,'',"fail hash{if} - value != 1");

    is($v,0,"closure not called yet");
    $match = $rule1->match("use");
    is($v,1,"closure was called hash{use}");

    $match = $rule1->match("untilaba123");
    #print Dump($match);
    is($match,'untilaba123',"subrule hash{until}");
    is($match->(),'untilaba123',"subrule hash{until} - 2");

}

{
    my $match;
    my %test = (
        rule1 => Pugs::Compiler::Regex->compile('xx %test yy'),  
        rule2 => Pugs::Compiler::Regex->compile('abc'),   
    );   
    $rule1 = Pugs::Compiler::Regex->compile('%test 123');
    #print $rule1->perl5;
    $match = $rule1->match("rule1xxrule2abcyy123");
    is($match,'rule1xxrule2abcyy123',"Matched hash inside hash");
}
