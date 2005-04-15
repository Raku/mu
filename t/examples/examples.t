#!/usr/bin/pugs

use v6;
require Test;

=pod

Test examples

This loads some of the scripts of the examples/ dir and compares their output
with a expected output.

=cut

my @examples = <
  fp/fp hanoi quicksort
  junctions/1 junctions/3 junctions/all-all junctions/all-any junctions/any-any
  junctions/any-any2 junctions/grades
>;

plan +@examples;

# We can't run under win32 because of C<\> as path separator instead of C</>
# -- awaiting v6 File::Spec
# Actually, nobody really needs the path separator

diag "Running under $?OS";

my ($pugs,$redir) = ("./pugs", ">");

if($?OS eq any(<MSWin32 mingw msys cygwin>)) {
  $pugs = 'pugs.exe';
  # $redir = '>';
};

for @examples -> $ex {
  my $command = "$pugs examples/$ex.p6 $redir temp-ex-output";
  diag $command;
  system $command;

  my $expected = slurp "examples/output/$ex";
  my $got      = slurp "temp-ex-output";
  unlink "temp-ex-output";

  is $got, $expected, "$ex.p6 worked";
}