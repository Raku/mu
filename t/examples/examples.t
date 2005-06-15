#!/usr/bin/pugs

use v6;
use Test;

=pod

Test examples

This loads some of the scripts of the examples/ dir and compares their output
with a expected output.

=cut

my @examples = <
  functional/fp algorithms/hanoi algorithms/quicksort
  junctions/1 junctions/3 junctions/all-all junctions/all-any junctions/any-any
  junctions/any-any2 junctions/grades
  vmethods/escape vmethods/bytes vmethods/time
>;

plan +@examples;
force_todo 1, 3;

# We can't run under win32 because of C<\> as path separator instead of C</>
# -- awaiting v6 File::Spec
# Actually, nobody really needs the path separator

diag "Running under $*OS";

my ($pugs,$redir) = ("./pugs", ">");

if($*OS eq any(<MSWin32 mingw msys cygwin>)) {
  $pugs = 'pugs.exe';
  # $redir = '>';
};

sub nonce () { return (".$*PID." ~ int rand 1000) }
for @examples -> $ex {
  my $fn = <temp-ex-output> ~ nonce;
  my $command = "$pugs examples/$ex.p6 $redir $fn";
  diag $command;
  system $command;

  my $expected = slurp "examples/output/$ex";
  my $got      = slurp $fn;
  unlink $fn;

  is $got, $expected, "$ex.p6 worked";
}
