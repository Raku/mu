use v6-alpha;

use Test;

plan 3;

# L<S04/"Conditional statements"/"Conditional statement modifiers"
#     "work as in Perl 5">

# test the ``while'' statement modifier
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
