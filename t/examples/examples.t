#!/usr/bin/pugs

use v6;
require Test;

=pod

Test examples

This loads some of the scripts of the examples/ dir and compares their output
with a expected output.

=cut

my @examples = <
  fp hanoi quicksort
  junctions/1 junctions/3 junctions/all-all junctions/all-any junctions/any-any
  junctions/any-any2 junctions/grades
>;

plan +@examples;

if $?OS eq "win32" {
  # We can't run under win32 because of C<\> as path separator instead of C</>
  # -- awaiting v6 File::Spec
  for @examples -> {
    skip "examples.t doesn't work under win32 yet";
  }
} else {
  for @examples -> $ex {
    system "./pugs examples/$ex.p6 &> temp-ex-output";

    my $expected = slurp "examples/output/$ex";
    my $got      = slurp "temp-ex-output";
    unlink "temp-ex-output";

    is $expected, $got, "$ex.p6 worked";
  }
}
