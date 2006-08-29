use v6-alpha;

use Test;

# Tests for post condition/iterating/looping including:
#  if, unless, for, while, until

# L<S04/"Conditional statements"/"Conditional statement modifiers"
#     "work as in Perl 5">

plan 18;

# postfix if
{
    my $a = 1;
    $a = 2 if 'a' eq 'a';
    is($a, 2, "post if");

    $a = 1;
    $a = 3 if 'a' eq 'b';
    is($a, 1, "post if");
}

# postfix unless
{
    my $a = 1;
    $a = 4 unless 'a' eq 'a';
    is($a, 1, "post unless");

    $a = 1;
    $a = 5 unless 'a' eq 'b';
    is($a, 5, "post unless");
}

# postfix for
{
    my $a;
    $a ~= $_ for ('a', 'b', 'a', 'b', 'a');
    is($a, "ababa", "post for with parens");
}

# post for without parens
{
    my $a;
    $a ~= $_ for 'a', 'b', 'a', 'b', 'a';
    is($a, "ababa", "post for without parens");
}

{
    my $a;
    $a += $_ for 1 .. 10;
    is($a, 55, "post for 1 .. 10");
}

{
    my $a;
    $a += $_ for 1 .. 10;
    is($a, 55, "post for 1 .. 10");
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

# postfix while
{
    my $a;
    my $b;
    $a += $b += 1 while $b < 10;
    is($a, 55, "post while");
}

{
    my @a = 'b'..'d';
    my $a = 'a';
    $a ~= ', ' ~ shift @a while @a;
    is($a, "a, b, c, d", "post while");
}

{
    my @a = 'a'..'e';
    my $a;
    ++$a while shift(@a) ne 'd';
    is($a, 3, "post while");
}

# postfix until
{
    my ($a, $b);
    $a += $b += 1 until $b >= 10;
    is($a, 55, "post until");
}

{
    my @a = ('a', 'b', 'a');
    my $a = 'b';
    $a ~= ', ' ~ shift @a until !+@a;
    is($a, "b, a, b, a", "post until");
}

{
    my @a = 'a'..'e';
    my $a;
    $a ++ until shift(@a) eq 'c';
    is($a, 2, "post until");
}
