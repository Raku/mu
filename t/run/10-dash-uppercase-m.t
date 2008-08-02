use v6;

use Test;

force_todo 4;

=begin pod

Test handling of C<-Mmodule>.

=end pod

my $dir = "t/run";

my @tests = (
  "-I$dir -MDummy -e load_test",
  "-e load_test -MDummy -I$dir",
# temporarily run below
#  "-I$dir -MDummy $dir/10-dash-uppercase-m.pl"
);

plan 4;
if $*OS eq "browser" {
  skip_rest "Programs running in browsers don't have access to regular IO.";
  exit;
}

diag "Running under $*OS";

my $redir = ">";

sub nonce () { return (".{$*PID}." ~ ((1..1000).pick) ~ ".tmp") }
sub run_pugs ($c) {
  my $tempfile = "temp-ex-output" ~ nonce;
  my $command = "$*EXECUTABLE_NAME $c $redir $tempfile";
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
is(run_pugs("-I$dir -MDummy $dir/10-dash-uppercase-m.pl"), "Module Dummy.pm was loaded\n", "Module was loaded", :todo<bug>);
