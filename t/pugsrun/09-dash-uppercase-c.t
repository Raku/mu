#!/usr/bin/pugs

use v6;
require Test;

=pod

Test handling of C<-Cbackend>.

=cut

my @tests = (
    '-CParrot -e1'
  , '-CPugs -e1'
  , '-CPugs t/pugsrun/09-dash-uppercase-c.t'
);

plan @tests*3;

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

my $dump_file = "dump.ast";
for @tests -> $test {
  my $output = run_pugs($test);
  is( $output, "", "No error output");

  my $f = slurp $dump_file;
  ok( defined $f, "dump.ast was filled" );
  
  ok( ?unlink($dump_file), "$dump_file was removed for next run"); # need loop for VMS
};

