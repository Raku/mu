#!/usr/bin/pugs

use v6;
require Test;

=pod

Test handling of C<-Cbackend>.

=cut

my @t_good = (
  '-C'
    ~ any('Pugs')
    ~ ' '
    ~ any('-e1', map( {"examples/$_.p6"}<
  fp
  hanoi
  junctions/1
  junctions/all-all
  junctions/3 junctions/all-any junctions/any-any
  junctions/any-any2 junctions/grades
  quicksort
>)), '-CParrot ' ~ any('-e1')
);

my @t_todo = map{$_.values} (map{$_.values} (
  '-C'
    ~ any('Parrot')
    ~ ' examples/'
    ~ any(<
  fp
  hanoi
  junctions/1
  junctions/3
  junctions/all-all
  junctions/all-any junctions/any-any
  junctions/any-any2 junctions/grades
  >) ~ '.p6'));

# I don't know (yet) how to force a junction into expansion
my (@tests_ok,@tests_todo);
for @t_good -> $test {
  push @tests_ok, $test;
};

for @t_todo -> $test {
  push @tests_todo, $test;
};


plan ((+@tests_ok+@tests_todo)*3);

diag "Running under $?OS";

# 2>&1 only works on WinNT upwards (cmd.exe) !
my ($pugs,$redir, $redir_stderr) = ("./pugs", ">", "2>&1");
if($?OS eq any<MSWin32 mingw cygwin>) {
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
  if (todo_is( $output, "", "No error output")) {

    my $f = slurp $dump_file;
    ok( defined $f, "dump.ast was created" );
    todo_ok( $f ~~ rx:perl5/.../, "... and it contains some output" );
  } else {
    todo_fail("No clean compile");
    todo_fail("No clean compile");
  };

  unlink($dump_file)
    or diag "$dump_file was not removed for next run";
};

