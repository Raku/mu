use v6-alpha;

use Test;

plan 15;

# L<S04/The do-once loop/"can't" put while or until modifier>
eval_dies_ok 'my $i; do { $i++ } while $i < 5;',
    "'do' can't take the 'while' modifier";

eval_dies_ok 'my $i; do { $i++ } until $i > 4;',
    "'do' can't take the 'until' modifier";

# L<S04/The do-once loop/statement "prefixing with" do>
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
    eval_is q{
        my $i;
        do {
            $i++;
            next;
            $i--;
        };
        $i;
    }, 1, "'next' works in 'do' block";
}

{
    eval_is q{
        my $i;
        do {
            $i++;
            last;
            $i--;
        };
        $i;
    }, 1, "'last' works in 'do' block";
}

{
    eval_is q{
        my $i;
        do {
            $i++;
            redo if $i < 3;
            $i--;
        };
        $i;
    }, 3, "'redo' works in 'do' block";
}

# L<S04/The do-once loop/"bare block" "no longer a do-once loop">
{
    eval_dies_ok 'my $i; { $i++; next; $i--; }',
        "bare block can't take 'next'";

    eval_dies_ok 'my $i; { $i++; last; $i--; }',
        "bare block can't take 'last'";
    
    eval_dies_ok 'my $i; { $i++; redo; $i--; }',
        "bare block can't take 'last'";
}

# L<S04/Statement parsing/"final closing curly on a line" 
#   reverts to semicolon>
{
    eval_is q{
        my $a = do {
            1 + 2;
        }  # no trailing `;'
        $a;
    }, 3, "final `}' on a line reverted to `;'";
}
