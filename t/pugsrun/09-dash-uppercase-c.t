#!/usr/bin/pugs

use v6;
require Test;

=pod

Test handling of C<-Cbackend>.

=cut

my @t = (
  '-C' 
    ~ any('Parrot','Pugs') 
    ~ ' ' 
    ~ any('-e1', map {"examples/$_.p6"},<
  fp hanoi quicksort
  junctions/1 junctions/3 junctions/all-all junctions/all-any junctions/any-any
  junctions/any-any2 junctions/grades
>)

);

# I don't know (yet) how to force a junction into expansion
my @tests;
for @t -> $test {     
  push @tests, $test;
};

plan +@tests*3;

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

