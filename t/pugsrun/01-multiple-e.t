#!/usr/bin/pugs

use v6;
require Test;

=pod

Test evaluation of multiple C<-e> switches.

Multiple C<-e> switches are supposed to work just
like C<join "\n"> concatenation .

=cut

my @examples = (
'-e print -e qq.Hello -e Pugs.'
);

plan +@examples;

diag "Running under $?OS";

my ($pugs,$redir) = ("./pugs", ">");

if ($?OS eq "MSWin32") {
  $pugs = 'pugs.exe';
  $redir = '>';
};

for @examples -> $ex {
  my $command = "$pugs $ex $redir temp-ex-output";
  diag $command;
  system $command;

  my $expected = "Hello\nPugs";
  my $got      = slurp "temp-ex-output";

  # $expected =~

  is $got, $expected, "Multiple -e switches work and append the script";
}
