#!/usr/bin/pugs

use v6;
use Test;

=pod

Tests for post condition/iterating/looping including:
  if, unless, for, while, until

=cut

plan 21;

my $str1 = "aaa";
my $str2 = "aaa";
my $str3 = "bbb";

{
    my $a = 1;
    $a = 2 if $str1 eq $str2;
    is($a, 2, "post if");

    $a = 1;
    $a = 3 if $str1 eq $str3;
    is($a, 1, "post if");

    $a = 1;
    $a = 4 unless $str2 eq $str1;
    is($a, 1, "post unless");

    $a = 1;
    $a = 5 unless $str2 eq $str3;
    is($a, 5, "post unless");
}

{
    my $a;
    $a ~= $_ for($str1, $str3, $str2, $str3, $str1);
    is($a, "aaabbbaaabbbaaa", "post for with parens");
}

{
    my $a;
    $a ~= $_ for $str1, $str3, $str2, $str3, $str1;
    is($a, "aaabbbaaabbbaaa", "post for without parens");
}

{
    my $a;
    $a += $_ for 1 .. 10;
    is($a, 55, "post for 1 .. 10");
}

{
    my $a;
    $a += $_ for(1 .. 10);
    is($a, 55, "post for(1 .. 10)");
}

{
    my @a = (5, 7, 9);
    my $a = 3;
    $a *= $_ for @a;
    is($a, 3 * 5 * 7 * 9, "post for array");
}

# Without this definition, you get:
#
# *** Undeclared variable: "&check"
#     at t/statements/statement_modifiers.t line 77, column 1
#
# see also t/pugsbugs/lexical_subs.t
my $size = 1;
sub check(Int $num) {
    fail("lexical subs are broken! :)") for 1..$size;
}

{
    my @a = (5, 7, 9);
    my $counter = 5;
    my sub check(Int $num){
        is($num, $counter, "sub Int with post for");
        $counter += 2;
    }
    check $_ for @a;
}

$size = 2;

{
    my @a = ($str1, $str2, $str3);
    my $counter = 0;
    my sub check(Str $str){
        ++$counter;
        is($str, "aaa", "sub Str with post for") if $counter <= 2;
        is($str, "bbb", "sub Str with post for") if $counter > 2;
    }
    check $_ for @a;
}

{
    my $a;
    my $b;
    $a += $b += 1 while $b < 10;
    is($a, 55, "post while");
}

{
    my @a = ($str1, $str3, $str2);
    my $a = $str3;
    $a ~= ', ' ~ shift @a while @a;
    is($a, "bbb, aaa, bbb, aaa", "post while");
}

{
    my @a = ($str1, $str2, $str1, $str2, $str3, $str2);
    my $a;
    ++$a while shift(@a) eq $str2;
    is($a, 4, "post while");
}

{
    my $a;
    my $b;
    $a += $b += 1 until $b >= 10;
    is($a, 55, "post until");
}

{
    my @a = ($str1, $str3, $str2);
    my $a = $str3;
    $a ~= ', ' ~ shift @a until !+@a;
    is($a, "bbb, aaa, bbb, aaa", "post until");
}

{
    my @a = ($str1, "ccc", "ddd", $str3, $str2);
    my $a;
    $a += 2 until shift(@a) eq $str3;
    is($a, 6, "post until");
}
