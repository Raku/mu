#!/usr/bin/pugs

use v6;
require Test;

=pod

Test evaluation of combination of C<-e> and C<-c> switches.

They should all do a syntax check and never evaluate the
C<-e> fragments.

=cut

my @examples;
push @examples,
  '-ce "print qq,Code got interpreted!,"',
  '-c -e "print qq,Code got interpreted!,"',
  '-e "print qq,Code got interpreted!," -c',
  '-eprint -c',
  '-ceprint'
  ;

plan +@examples;

diag "Running under $?OS";

my ($pugs,$redir) = ("./pugs", ">");

if($?OS eq any<MSWin32 mingw cygwin>) {
  $pugs = 'pugs.exe';
  $redir = '>';
};

for @examples -> $ex {
  my $command = "$pugs $ex $redir temp-ex-output";
  diag $command;
  system $command;

  my $expected = "-e syntax OK\n";
  my $got      = slurp "temp-ex-output";
  unlink "temp-ex-output";

  is $got, $expected, "$ex works";
}
