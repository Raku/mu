#!/usr/bin/pugs

use v6;
use Test;

=pod

Test handling of C<-Cbackend>.

=cut

sub flatten (Any|Junction $x) {
    ($x.isa(Junction)) ?? map &flatten, $x.values !! $x
}

my @t_good = map &flatten, (
  any('-C')
    ~ any('Pugs', 'pUGs')
    ~ ' '
    ~ any('-e1', map { "examples/$_.p6" } <
  functional/fp
  algorithms/hanoi
  junctions/1
  junctions/all-all
  junctions/3 junctions/all-any junctions/any-any
  junctions/any-any2 junctions/grades
  algorithms/quicksort
>),
  any('-C')
    ~ any('Parrot', 'paRRot')
    ~ ' '
    ~ any('-e1', map {"examples/$_.p6"} <
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
my ($pugs,$redir, $redir_stderr) = ("./pugs", ">");
if($*OS eq any(<MSWin32 mingw msys cygwin>)) {
  $pugs = 'pugs.exe';
};

sub nonce () { return (".$*PID." ~ int rand 1000) }
sub run_pugs ($c) {
  my $tempfile = "temp-ex-output" ~ nonce;
  my $command = "$pugs $c $redir $tempfile";
  diag $command;
  system $command;
  my $res = slurp $tempfile;
  unlink $tempfile;
  return $res;
}

my $dump_file = "dump.ast";

for @tests_ok -> $test {

  my $fh = open("$dump_file", :w);
  $fh.close();

  my $f = run_pugs($test);
  ok( defined $f, "dump file was created" );
  ok( $f ~~ rx:perl5/.../, "... and it contains some output" );

  unlink($dump_file)
    or diag "$dump_file was not removed for next run";
};

