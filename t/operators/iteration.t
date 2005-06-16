#!/usr/bin/pugs

use v6;
use Test;

# Tests the generic "=" prefix operator.
# See http://www.nntp.perl.org/group/perl.perl6.language/21895.

plan 7;

{
  my $was_in_next;
  my @elems;

  class MySimpleIterClass {
    method next () {
      $was_in_next++;
      return @elems.shift;
    }
  }

  my $obj = MySimpleIterClass.new();

  @elems = <a b c d>;
  $was_in_next = 0;
  is ~(1..5).map:{ $obj.next() }, "a b c d ",
    "manually calling .next on own object works (1)";
  is $was_in_next, 5,
    "manually calling .next on own object works (2)";
  is +@elems, 0,
    "manually calling .next on own object works (3)";

  @elems = <a b c d>;
  $was_in_next = 0;
  is =$obj, "a", '&prefix:<=> (generic iteration operator) works (1)';
  is ~(1..4).map:{ =$obj }, "b c d ",
    '&prefix:<=> (generic iteration operator) works (2)';
  is $was_in_next, 5,
    '&prefix:<=> (generic iteration operator) works (2)';
  is +@elems, 0,
    '&prefix:<=> (generic iteration operator) works (3)';
}
