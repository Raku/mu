#!/usr/bin/pugs

use v6;
require Test;

=pod

Test evaluation of multiple C<-e> switches.

Multiple C<-e> switches are supposed to work just
like C<join "\n"> concatenation .

=cut

my @examples;
push @examples, 
  '-ce "print qq<Code got interpreted!>"',
  '-c -e "print qq<Code got interpreted!>"', 
  '-e "print qq<Code got interpreted!>" -c',
  '-eprint -c',
  '-ceprint'
  ;

plan +@examples;

diag "Running under $?OS";

my ($pugs,$redir) = ("./pugs", "&>");

if ($?OS eq "MSWin32") {
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

  todo_is $got, $expected, "$ex works";
}
