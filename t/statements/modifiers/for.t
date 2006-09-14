use v6-alpha;

use Test;

plan 12;

# L<S04/"Conditional statements"/"Conditional statement modifiers"
#     "work as in Perl 5">

# test the for statement modifier
{
    my $a;
    $a ~= $_ for ('a', 'b', 'a', 'b', 'a');
    is($a, "ababa", "post for with parens");
}

# without parens
{
    my $a;
    $a ~= $_ for 'a', 'b', 'a', 'b', 'a';
    is($a, "ababa", "post for without parens");
}

{
    my $a;
    $a += $_ for (1 .. 10);
    is($a, 55, "post for 1 .. 10 with parens");
}

{
    my $a;
    $a += $_ for 1 .. 10;
    is($a, 55, "post for 1 .. 10 without parens");
}

{
    my @a = (5, 7, 9);
    my $a = 3;
    $a *= $_ for @a;
    is($a, 3 * 5 * 7 * 9, "post for array");
}

{
    my @a = (5, 7, 9);
    my $i = 5;
    my sub check(Int $n){
        is($n, $i, "sub Int with post for");
        $i += 2;
    }
    check $_ for @a;
}

# The following tests are all legal syntactically, but neither
# of these do anything other than produce a closure muliple
# times without calling it.
# See Larry's clarification on p6l:
# L<http://www.nntp.perl.org/group/perl.perl6.language/26071>

{
    my $a;
    { $a++ } for 1..3;
    is $a, undef, 'the closure was never called';
}

{
    my $a;
    -> $i { $a += $i } for 1..3;
    is $a, undef, 'the closure was never called';
}

# L<S04/The C<for> statement/"for" use a private instance of $_>
{
    my $i;
    $_ = 10;
    $i += $_ for 1..3;
    is $_, 10, 'outer $_ did not get updated in lhs of for';
    is $i, 1+2+3, 'postfix for worked';
}
