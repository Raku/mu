use v6-alpha;

use Test;

=pod

L<S03/"New operators" /Binary ===>

C<===> and C<eqv> are 2 distinct operators, where C<===> tests value
equivalence for immutable types and reference equivalence for 
mutable types, and C<eqv> tests value equivalence for snapshots of mutable
types.  So C<(1,2) === (1,2)> returns true but C<[1,2] === [1,2]> returns 
false, and C<[1,2] eqv [1,2]> returns true.

=cut

plan 76;

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

  ok  (\@a === \@a), "=== on array references (1)", :todo<bug>;
  ok  (\@b === \@b), "=== on array references (2)", :todo<bug>;
  ok !(\@a === \@b), "=== on array references (3)";
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
  ok !([1,2,3] === [1,2,3]), "=== on anonymous array references (2)";
  ok !([]      === []),      "=== on anonymous array references (3)";
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

# eqv on values
{
  ok  (1 eqv 1), "eqv on values (1)";
  ok  (0 eqv 0), "eqv on values (2)";
  ok !(0 eqv 1), "eqv on values (3)";
}

# Value types
{
  my $a = 1;
  my $b = 1;

  ok $a eqv $a, "eqv on value types (1-1)";
  ok $b eqv $b, "eqv on value types (1-2)";
  ok $a eqv $b, "eqv on value types (1-3)";
}

{
  my $a = 1;
  my $b = 2;

  ok  ($a eqv $a), "eqv on value types (2-1)";
  ok  ($b eqv $b), "eqv on value types (2-2)";
  ok !($a eqv $b), "eqv on value types (2-3)";
}

# Reference types
{
  my @a = (1,2,3);
  my @b = (1,2,3);

  ok  (\@a eqv \@a), "eqv on array references (1)";
  ok  (\@b eqv \@b), "eqv on array references (2)";
  ok !(\@a eqv \@b), "eqv on array references (3)", :todo<bug>;
}

{
  my $a = \3;
  my $b = \3;

  ok  ($a eqv $a), "eqv on scalar references (1-1)";
  ok  ($b eqv $b), "eqv on scalar references (1-2)";
  ok !($a eqv $b), "eqv on scalar references (1-3)", :todo<bug>;
}

{
  my $a = { 3 };
  my $b = { 3 };

  ok  ($a eqv $a), "eqv on sub references (1-1)";
  ok  ($b eqv $b), "eqv on sub references (1-2)";
  ok !($a eqv $b), "eqv on sub references (1-3)";
}

{
  ok  (&say eqv &say), "eqv on sub references (2-1)";
  ok  (&map eqv &map), "eqv on sub references (2-2)";
  ok !(&say eqv &map), "eqv on sub references (2-3)";
}

{
  my $num = 3;
  my $a   = \$num;
  my $b   = \$num;

  ok  ($a eqv $a), "eqv on scalar references (2-1)";
  ok  ($b eqv $b), "eqv on scalar references (2-2)";
  ok  ($a eqv $b), "eqv on scalar references (2-3)";
}

{
  ok !([1,2,3] eqv [4,5,6]), "eqv on anonymous array references (1)";
  ok !([1,2,3] eqv [1,2,3]), "eqv on anonymous array references (2)", :todo<bug>;
  ok !([]      eqv []),      "eqv on anonymous array references (3)", :todo<bug>;
}

{
  ok !({a => 1} eqv {a => 2}), "eqv on anonymous hash references (1)";
  ok !({a => 1} eqv {a => 1}), "eqv on anonymous hash references (2)";
}

{
  ok !(\3 eqv \4),         "eqv on anonymous scalar references (1)";
  ok !(\3 eqv \3),         "eqv on anonymous scalar references (2)", :todo<bug>;
  ok !(\undef eqv \undef), "eqv on anonymous scalar references (3)", :todo<bug>;
}

# Chained eqv (not specced, but obvious)
{
  ok  (3 eqv 3 eqv 3), "chained eqv (1)";
  ok !(3 eqv 3 eqv 4), "chained eqv (2)";
}

# Subparam binding doesn't affect eqv test
{
  my $foo;
  my $test = -> $arg { $foo eqv $arg };

  $foo = 3;
  ok  $test($foo), "subparam binding doesn't affect eqv (1)";
  ok  $test(3),    "subparam binding doesn't affect eqv (2)";

  ok !$test(4),    "subparam binding doesn't affect eqv (3)";
  my $bar = 4;
  ok !$test($bar), "subparam binding doesn't affect eqv (4)";
}
