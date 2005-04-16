#!/usr/bin/pugs

use v6;
require Test;

=pod

Test handling of C<-Mmodule>.

=cut

push @INC, <  blib6/lib >; # ext/File-Spec/lib
require File::Spec;

my $dir = catdir( <t pugsrun> );

my @tests = (
  "-I$dir -MDummy -e load_test",
  "-e load_test -MDummy -I$dir"
);

plan 1+@tests;

diag "Running under $*OS";

my ($pugs,$redir) = ("./pugs", ">");
if($*OS eq any<MSWin32 mingw msys cygwin>) {
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

