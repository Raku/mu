use v6;

use Test;

=begin pod

Test handling of C<-Cbackend>.

=end pod

sub flatten (Any|Junction $x) {
    ($x.isa(Junction)) ?? map &flatten, $x.values !! $x
}

my @t_good = map &flatten, (
  any('-C')
    ~ any('Pugs', 'pUGs')
    ~ ' '
    ~ any('-e1', map { "examples/$_.pl" }, <
  functional/fp
  algorithms/hanoi
  junctions/1
  junctions/all-all
  junctions/3 junctions/all-any junctions/any-any
  junctions/any-any2 junctions/grades
  algorithms/quicksort
>),
  any('-C')
    ~ any('PIR', 'pir')
    ~ ' '
    ~ any('-e1', map {"examples/$_.pl"}, <
  junctions/1
  junctions/any-any
  junctions/any-any2
  junctions/3
  junctions/all-all
  junctions/grades
  functional/fp
  algorithms/hanoi
  junctions/all-any
>)
);

# I don't know (yet) how to force a junction into expansion
my (@tests_ok);
for @t_good -> $test {
  push @tests_ok, $test;
};


plan((+@tests_ok)*2);
if $*OS eq "browser" {
  skip_rest "Programs running in browsers don't have access to regular IO.";
  exit;
}

diag "Running under $*OS";

# 2>&1 only works on WinNT upwards (cmd.exe) !
my $redir_stderr = ">";

sub nonce () { return (".{$*PID}." ~ (1..1000).pick) }
sub run_pugs ($c) {
  my $tempfile = "temp-ex-output" ~ nonce;
  my $command = "$*EXECUTABLE_NAME $c $redir_stderr $tempfile";
  diag $command;
  system $command;
  my $res = $tempfile ~~ :s;
  unlink $tempfile;
  return $res;
}

for @tests_ok -> $test {
  my $f = run_pugs($test);
  ok( defined $f, "dump file was created" );
  ok( $f >= 3, "... and it contains some output" );
};

