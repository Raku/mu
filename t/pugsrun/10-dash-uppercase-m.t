#!/usr/bin/pugs

use v6;
use Test;

force_todo 4;

=pod

Test handling of C<-Mmodule>.

=cut


BEGIN { push @INC, < blib6/lib > } # ext/File-Spec/lib
require File::Spec;

my $dir = catdir( <t pugsrun> );

my @tests = (
  "-I$dir -MDummy -e load_test",
  "-e load_test -MDummy -I$dir",
# temporarily run below
#  "-I$dir -MDummy $dir/10-dash-uppercase-m.p6"
);

plan 1+@tests;

diag "Running under $*OS";

my ($pugs,$redir) = ("./pugs", ">");
if($*OS eq any<MSWin32 mingw msys cygwin>) {
  $pugs = 'pugs.exe';
};

sub nonce () { return (".$*PID." ~ (int rand 1000) ~ ".tmp") }
sub run_pugs ($c) {
  my $tempfile = "temp-ex-output" ~ nonce;
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
  is( $output, "Module Dummy.pm was loaded\n", "Module was loaded");
};

# when this works, delete this line, and uncomment this case in the list above.
is(run_pugs("-I$dir -MDummy $dir/10-dash-uppercase-m.p6"), "Module Dummy.pm was loaded\n", "Module was loaded", :todo<bug>);
