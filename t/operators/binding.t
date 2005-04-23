#!/usr/bin/pugs

use v6;
require Test;

=kwid

=head1 Binding tests

These tests are derived from the "Binding" section of Synopsis 3

L<S03/"Binding">

=cut

plan 21;

# L<S03/"Binding" /replaces the container itself.  For instance:/>

my $x = 'Just Another';
is($x, 'Just Another', 'normal assignment works');

my $y := $x;
is($y, 'Just Another', 'y is now bound to x');

ok(eval '$y =:= $x', 'y is bound to x (we checked with the =:= identity op)', :todo(1));

my $z = $x;
is($z, 'Just Another', 'z is not bound to x');

ok(eval '!($z =:= $x)', 'z is not bound to x (we checked with the =:= identity op)', :todo(1));

$y = 'Perl Hacker';
is($y, 'Perl Hacker', 'y has been changed to "Perl Hacker"');
is($x, 'Perl Hacker', 'x has also been changed to "Perl Hacker"');

is($z, 'Just Another', 'z is still "Just Another" because it was not bound to x');

sub bar {
  return $CALLER::a eq $CALLER::b;
}

sub foo {
  my $a = "foo";
  my $b := $a;
  return bar(); # && bar2();
}

ok(foo(), "CALLER resolves bindings in caller's dynamic scope");

# Binding to swap
{
  my $a = "a";
  my $b = "b";
  ($a, $b) := ($b,$a);
  is($a, 'b', '$a has been changed to "b"');
  is($b, 'a', '$b has been changed to "a"');
}

# Binding subroutine parameters
# XXX! When executed in interactive Pugs, the following test works!
{
  my $a;
  my $b = sub($arg) { $a := $arg };
  my $val = 42;

  $b($val);
  is $a, 42, "bound readonly sub param was bound correctly (1)";
  $val++;
  is $a, 43, "bound readonly sub param was bound correctly (2)";

  throws_ok { $a = 23 }, "Can't modify constant item",
    "bound readonly sub param remains readonly (1)";
  is $a, 43,
    "bound readonly sub param remains readonly (2)";
  is $val, 43,
    "bound readonly sub param remains readonly (3)";
}

{
  my $a;
  my $b = sub($arg is rw) { $a := $arg };
  my $val = 42;

  $b($val);
  is $a, 42, "bound rw sub param was bound correctly (1)";
  $val++;
  is $a, 43, "bound rw sub param was bound correctly (2)";

  lives_ok { $a = 23 }, "bound rw sub param remains rw (1)";
  is $a, 23,            "bound rw sub param remains rw (2)";
  is $val, 23,          "bound rw sub param remains rw (3)";
}
