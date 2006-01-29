#!/usr/bin/pugs

use v6;
use Test;

force_todo 4;

=pod

Test handling of C<-Mmodule>.

=cut

my $dir = "t/pugsrun";

my @tests = (
  "-I$dir -MDummy -e load_test",
  "-e load_test -MDummy -I$dir",
# temporarily run below
#  "-I$dir -MDummy $dir/10-dash-uppercase-m.p6"
);

plan 4;
if $*OS eq "browser" {
  skip_rest "Programs running in browsers don't have access to regular IO.";
  exit;
}

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
require Dummy; pass "(dummy instead of broken use_ok)";

for @tests -> $test {
  my $output = run_pugs($test);
  is( $output, "Module Dummy.pm was loaded\n", "Module was loaded");
};

# when this works, delete this line, and uncomment this case in the list above.
is(run_pugs("-I$dir -MDummy $dir/10-dash-uppercase-m.p6"), "Module Dummy.pm was loaded\n", "Module was loaded", :todo<bug>);
