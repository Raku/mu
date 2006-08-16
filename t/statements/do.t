use v6-alpha;

use Test;

plan 16;

# L<S04/The do-once loop/"can't" put while or until modifier>
{
    my $i;
    ok !eval 'do { $i++ } while $i < 5;',
        "'do' can't take the 'while' modifier";
    is $i, undef, 'the code never run';
}

{
    my $i;
    ok !eval 'do { $i++ } until $i > 4;',
        "'do' can't take the 'until' modifier";
    is $i, undef, 'the code never run';
}

# L<S04/The do-once loop/statements "prefixing with" do>
{
    my $x;
    my ($a, $b, $c) = 'a' .. 'c';

    $x = do if $a { $b } else { $c };
    is $x, 'b', "prefixing 'if' statement with 'do' (then)";

    $x = do if !$a { $b } else { $c };
    is $x, 'c', "prefixing 'if' statement with 'do' (else)";
}

{
    my $ret = do given 3 {
        when 3 { 1 }
    };
    is($ret, 1, 'do STMT works');
}

{
    my $ret = do { given 3 {
        when 3 { 1 }
    } };
    is($ret, 1, 'do { STMT } works');
}

# L<S04/The do-once loop/"you may use" do "on an expression">
{
    my $ret = do 42;
    is($ret, 42, 'do EXPR should also work (single number)');

    $ret = do 3 + 2;
    is($ret, 5, 'do EXPR should also work (simple + expr)');
}

# L<S04/The do-once loop/"can take" "loop control statements">
{
    my $i;
    do {
        $i++;
        next;
        $i--;
    }
    is $i, 1, "'next' works in 'do' block";
}

{
    my $i;
    do {
        $i++;
        last;
        $i--;
    }
    is $i, 1, "'last' works in 'do' block";
}

# L<S04/The do-once loop/"bare block" "no longer a do-once loop">
{
    my $i;
    ok !eval('{ $i++; next; $i--; }'), "bare block can't take 'next'";
    is $i, undef, "the code snippet never run";
}

{
    my $i;
    ok !eval('{ $i++; last; $i--; }'), "bare block can't take 'last'";
    is $i, undef, "the code snippet never run";
}
