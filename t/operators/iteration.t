#!/usr/bin/pugs

use v6;
use Test;

# Tests the generic "=" prefix operator.
# See thread "PATCH: S04 - unary C<=> is not slurpy" from Patrick R. Michaud,
# especially Damian's reply at
# L<"http://www.nntp.perl.org/group/perl.perl6.language/21895">.

# Update: L<"http://use.perl.org/~autrijus/journal/25337">
# &prefix:<=> is just .shift in item context; in slurpy context it just turns
# the iterator into a generator. All arrays are concatenations of generators
# (which may or may not be preflattened)

plan 7;

{
  my $was_in_next;
  my @elems;

  class MySimpleIterClass {
    method shift () {
      $was_in_next++;
      return pop @elems;
    }
  }

  my $obj = MySimpleIterClass.new();

  @elems = <a b c d>;
  $was_in_next = 0;
  is ~(1..5).map:{ $obj.shift() }, "d c b a ",
    "manually calling .next on own object works (1)";
  is $was_in_next, 5,
    "manually calling .next on own object works (2)";
  is +@elems, 0,
    "manually calling .next on own object works (3)";

  @elems = <a b c d>;
  $was_in_next = 0;
  is =$obj, "d", '&prefix:<=> (generic iteration operator) works (1)';
  is ~(1..4).map:{ =$obj }, "c b a ",
    '&prefix:<=> (generic iteration operator) works (2)';
  is $was_in_next, 5,
    '&prefix:<=> (generic iteration operator) works (2)';
  is +@elems, 0,
    '&prefix:<=> (generic iteration operator) works (3)';
}
