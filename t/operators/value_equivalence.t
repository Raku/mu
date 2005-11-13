#!/usr/bin/pugs

use v6;
use Test;

=pod

This is going to be in S03 (mail from Luke to p6l:
L<"http://www.nntp.perl.org/group/perl.perl6.language/22849">).
L<S03/"New operators" /Binary C<===>/>
Binary C<eqv> tests value equivalence: for two value types, tests whether they
are the same value (eg. C<1 eqv 1>); for two reference types, checks whether
they are the same reference (eg. it is not true that C<[1,2] eqv [1,2]>, but it
is true that C<\@a eqv \@a>).

Update (2005-11-13): C<eqv> is now spelled C<===>.

=cut

plan 38;

# === on values
{
  ok  (1 === 1), "=== on values (1)";
  ok  (0 === 0), "=== on values (2)";
  ok !(0 === 1), "=== on values (3)";
}

# Value types
{
  my $a = 1;
  my $b = 1;

  ok $a === $a, "=== on value types (1-1)";
  ok $b === $b, "=== on value types (1-2)";
  ok $a === $b, "=== on value types (1-3)";
}

{
  my $a = 1;
  my $b = 2;

  ok  ($a === $a), "=== on value types (2-1)";
  ok  ($b === $b), "=== on value types (2-2)";
  ok !($a === $b), "=== on value types (2-3)";
}

# Reference types
{
  my @a = (1,2,3);
  my @b = (1,2,3);

  ok  (\@a === \@a), "=== on array references (1)";
  ok  (\@b === \@b), "=== on array references (2)";
  ok !(\@a === \@b), "=== on array references (3)", :todo<bug>;
}

{
  my $a = \3;
  my $b = \3;

  ok  ($a === $a), "=== on scalar references (1-1)";
  ok  ($b === $b), "=== on scalar references (1-2)";
  ok !($a === $b), "=== on scalar references (1-3)", :todo<bug>;
}

{
  my $a = { 3 };
  my $b = { 3 };

  ok  ($a === $a), "=== on sub references (1-1)";
  ok  ($b === $b), "=== on sub references (1-2)";
  ok !($a === $b), "=== on sub references (1-3)";
}

{
  ok  (&say === &say), "=== on sub references (2-1)";
  ok  (&map === &map), "=== on sub references (2-2)";
  ok !(&say === &map), "=== on sub references (2-3)";
}

{
  my $num = 3;
  my $a   = \$num;
  my $b   = \$num;

  ok  ($a === $a), "=== on scalar references (2-1)";
  ok  ($b === $b), "=== on scalar references (2-2)";
  ok  ($a === $b), "=== on scalar references (2-3)";
}

{
  ok !([1,2,3] === [4,5,6]), "=== on anonymous array references (1)";
  ok !([1,2,3] === [1,2,3]), "=== on anonymous array references (2)", :todo<bug>;
  ok !([]      === []),      "=== on anonymous array references (3)", :todo<bug>;
}

{
  ok !({a => 1} === {a => 2}), "=== on anonymous hash references (1)";
  ok !({a => 1} === {a => 1}), "=== on anonymous hash references (2)";
}

{
  ok !(\3 === \4),         "=== on anonymous scalar references (1)";
  ok !(\3 === \3),         "=== on anonymous scalar references (2)", :todo<bug>;
  ok !(\undef === \undef), "=== on anonymous scalar references (3)", :todo<bug>;
}

# Chained === (not specced, but obvious)
{
  ok  (3 === 3 === 3), "chained === (1)";
  ok !(3 === 3 === 4), "chained === (2)";
}

# Subparam binding doesn't affect === test
{
  my $foo;
  my $test = -> $arg { $foo === $arg };

  $foo = 3;
  ok  $test($foo), "subparam binding doesn't affect === (1)";
  ok  $test(3),    "subparam binding doesn't affect === (2)";

  ok !$test(4),    "subparam binding doesn't affect === (3)";
  my $bar = 4;
  ok !$test($bar), "subparam binding doesn't affect === (4)";
}
