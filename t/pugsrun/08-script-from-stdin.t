#!/usr/bin/pugs

use v6;
require Test;

=pod

Test evaluation a script read from STDIN, as
indicated by the C<-> switch.

=cut

my @examples = (
   'print qq.Hello Pugs.'
);

plan +@examples;

diag "Running under $?OS";

my ($pugs,$redir,$echo) = ("./pugs", ">", "echo");

if ($?OS eq "MSWin32") {
  $pugs = 'pugs.exe';
};

for @examples -> $ex {
  my $command = "$echo $ex | $pugs - $redir temp-ex-output";
  diag $command;
  system $command;

  my $expected = "Hello Pugs";
  my $got      = slurp "temp-ex-output";

  is $got, $expected, "Multiple -e switches work and append the script";
}