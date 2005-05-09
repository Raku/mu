#!/usr/bin/pugs

use v6;
use Test;

=pod

Test handling of C<-Cbackend>.

=cut
sub flatten (Any|Junction $x) {
    ($x.isa('Junction')) ?? map &flatten, $x.values :: $x
}

my @t_good = map &flatten, (
  any('-C')
    ~ any('Pugs')
    ~ ' '
    ~ any('-e1', map( {"examples/$_.p6"}<
  functional/fp
  hanoi
  junctions/1
  junctions/all-all
  junctions/3 junctions/all-any junctions/any-any
  junctions/any-any2 junctions/grades
  quicksort
>)), '-CParrot ' ~ any('-e1', map( {"examples/$_.p6"}<
  junctions/1
  junctions/any-any
  junctions/any-any2
  junctions/3
  junctions/all-all
  junctions/grades
>))
);

my @t_todo = map &flatten, (
  '-C'
    ~ any('Parrot')
    ~ ' examples/'
    ~ any(<
  functional/fp
  hanoi
  junctions/all-any
  >) ~ '.p6'
);

# I don't know (yet) how to force a junction into expansion
my (@tests_ok,@tests_todo);
for @t_good -> $test {
  push @tests_ok, $test;
};

for @t_todo -> $test {
  push @tests_todo, $test;
};


plan ((+@tests_ok+@tests_todo)*3);

diag "Running under $*OS";

# 2>&1 only works on WinNT upwards (cmd.exe) !
my ($pugs,$redir, $redir_stderr) = ("./pugs", ">", "2>&1");
if($*OS eq any(<MSWin32 mingw msys cygwin>)) {
  $pugs = 'pugs.exe';
};

sub run_pugs ($c) {
  my $tempfile = "temp-ex-output";
  my $command = "$pugs $c $redir $tempfile $redir_stderr";
  diag $command;
  system $command;
  my $res = slurp $tempfile;
  unlink $tempfile;
  return $res;
}

my $dump_file = "dump.ast";

for @tests_ok -> $test {

  my $fh = open( ">$dump_file" );
  $fh.close();

  my $output = run_pugs($test);
  is( $output, "", "No error output");

  my $f = slurp $dump_file;
  ok( defined $f, "dump.ast was created" );
  ok( $f ~~ rx:perl5/.../, "... and it contains some output" );

  unlink($dump_file)
    or diag "$dump_file was not removed for next run";
};

for @tests_todo -> $test {

  my $fh = open( ">$dump_file" );
  $fh.close();

  my $output = run_pugs($test);
  if (is( $output, "", "No error output", :todo)) {

    my $f = slurp $dump_file;
    ok( defined $f, "dump.ast was created" );
    ok( $f ~~ rx:perl5/.../, "... and it contains some output" , :todo);
  } else {
    fail("No clean compile", :todo);
    fail("No clean compile", :todo);
  };

  unlink($dump_file)
    or diag "$dump_file was not removed for next run";
};