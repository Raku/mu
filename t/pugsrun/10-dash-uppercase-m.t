#!/usr/bin/pugs

use v6;
require Test;

=pod

Test handling of C<-Mmodule>.

=cut

require File::Spec;

my $dir = catdir( <t pugsrun> );

my @t = (
  "-I$dir -MDummy -e load_test",
  "-e load_test -MDummy -I$dir"
);

# I don't know (yet) how to force a junction into expansion
my @tests;
for @t -> $test {
  push @tests, $test;
};

plan 1+@tests;

diag "Running under $?OS";

my ($pugs,$redir) = ("./pugs", ">");
if ($?OS eq "MSWin32") {
  $pugs = 'pugs.exe';
};

sub run_pugs ($c) {
  my $tempfile = "temp-ex-output";
  my $command = "$pugs $c $redir $tempfile";
  diag $command;
  system $command;
  my $res = slurp $tempfile;
  unlink $tempfile;
  return $res;
}

push @*INC, $dir;
use_ok('Dummy');

for @tests -> $test {
  my $output = run_pugs($test);
  is( $output, "Module was loaded\n", "Module was loaded");
};

