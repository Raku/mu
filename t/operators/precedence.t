#!/usr/bin/pugs

use v6;
use Test;

=pod

Tests that each level bind tighter than the next by sampling some ops.

In between each precedence level are some tests that demonstrate the
proper separation of the two levels.

L<S03/"Precedence">

=cut

plan 41;


# 1. terms

# FIXME how do we test this?

# 2. postfix method

# this wants objects, but maybe it can also work like
# @sort.map binds tighter than ++ somehow?

# 3. autoincrement

my $i = 2;
is(++$i ** 2, 9, "++ bind tighter than **");
is(--$i ** 2, 4, "-- does too");

# 4. exponentiation

is(-2**2, -4, "** bind tighter than unary -");
isa_ok(~2**4, "Str", "~4**4 is a string");

# 5. symbolic unary

is(-2 x 2, "-2-2", "unary - binds tighter than x");
is(-(2 x 2), "-22", "beh");
is(?2*2, 2, "binary -> numify causes reinterpretation as, binds tighter than *");

# 6. multiplicative

is(2 x 2 + 10, 32, "x bind tighter than binary +");
is(4 + 3 * 2, 10, "* binds tighter than binary +");
is(0 - 3 / 3, -1, "/ bind tighter than binary -");

# 7. additive

is(1 ~ 2 * 3, 16, "~ binds looser than *");
ok((1 ~ 2 & 12) == 12, "but tighter than &");
ok((2 + 2 | 4) == 4, "and + binds tigher than |");

# 8. junctive and

ok(       (1 & 2 | 3) !=3, '& binds tighter than |');
ok((!(1 & 2 | 3) < 2), "ditto");
ok(((1 & 2 ^ 3) < 3), "and also ^");
ok(     !(1 & 2 ^ 4) != 3, "blah blah blah");

# 9. junctive or

{ # test that | and ^ are on the same level
    my $a = (1 | 2 ^ 3);
    my $b = (1 ^ 2 | 3);

    ok($a == 3, "only one is eq 3");
    ok($a != 3, "either is ne 3");
    ok($a == 1, "either is eq 1");
    ok($b == 2, "either is eq 2, ne 3");
    ok($b == 1, "either is eq 1");
    ok($b == 3, "either is eq 3, of which only one is");
    ok(!($b != 3), "1 is ne 3, and (2 | 3) is both ne 3 and eq 3, so it's ne, so 1 ^ 2 | 3");
};

{
    my $a = (abs -1 ^ -1); # read as abs(-1 ^ -1) -> (1^1)
    ok(!($a == 1), 'junctive or binds more tightly then abs (1)');

    my $b = ((abs -1) ^ -1); # -> (1 ^ -1)
    ok($b == 1, "this is true because only one is == 1");
};

# 10. named unary

is((abs -1 .. 3), (1 .. 3), "abs binds tighter than ..");
is((rand 3 <=> 5), -1, "rand binds tighter than <=>");

# 11. nonchaining binary

ok(0 < 2 <=> 1 < 2, "0 < 2 <=> 1 < 2 means 0 < 1 < 2");

# 12. chaining binary

is((0 != 1 && "foo"), "foo", "!= binds tigher than &&");
ok((0 || 1 == (2-1) == (0+1) || "foo") ne "foo", "== binds tigher than || also when chaning");

# 13. tight and (&&)

# 14. tight or (||, ^^, //)

is((1 && 0 ?? 2 :: 3), 3, "&& binds tighter than ??");
### FIXME - need also ||, otherwise we don't prove || and ?? are diff

# 15. ternary

{
    my $a = 0 ?? "yes" :: "no";
    is($a, "no", "??:: binds tighter than =");
#    (my $b = 1) ?? "true" :: "false";
#    is($b, 1, "?? :: just thrown away with = in parens");
};


# 16. assignment

{
    my @c = 1, 2, 3;
    is(@c, (1), "= binds tighter than , (*sigh*)", :todo);
    my @a = (1, 3) ¥ (2, 4);
    is(@a, [1, 3], "= binds tighter than yen", :todo);
};

{
    my @b = ((1, 3) ¥ (2, 4));
    is(@b, [1 .. 4], "parens work around this");
};

# 17. list item separator

{
    my @d;
    eval_ok '@d <== (1, 3) ¥ (2, 4), "left pointing pipe parses"', :todo;
    is(@d, [1 .. 4], "to complicate things further, left pointing pipe *does* DWIM", :todo);
    my $c = any 1, 2, 3;
    ok($c == 2, "any is less tight than comma");
}

# 18. rightward list op

{
    my @e; eval '@e = (map { $_+1 } <== (1, 2, 3) ==> map { $_*2 })'; # =D
    is(@e, [4, 6, 8], "<== is tighter than ==>", :todo);
}

# 19. pipe forward

# 20. loose and

# 21. loose or

# 22. expr terminator

# 23. uc|ucfirst|lc|lcfirst
# t/builtins/strings/uc|ucfirst|lc|lcfirst.t didn't compile because of this bug.
# Compare:
#   $ perl -we 'print uc "a" eq "A"'
#   1
# opposed to Pugs parses it:
#   $ perl -we 'print uc("a" eq "A")'
#   $    (no output)
ok (uc "a" eq "A"), "uc has the correct precedence in comparision to eq";
