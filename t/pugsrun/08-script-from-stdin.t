#!/usr/bin/pugs

use v6;
require Test;

=pod

Test evaluation a script read from STDIN, as
indicated by the C<-> switch.

=cut

my @examples = map { $_.values() } (
   any('print qq.Hello Pugs.',
       'print @ARGS',
   )
);

plan +@examples;

diag "Running under $?OS";

my ($pugs,$redir,$echo) = ("./pugs", ">", "echo");

if($?OS eq any<MSWin32 mingw cygwin>) {
  $pugs = 'pugs.exe';
};

for @examples -> $ex {
  my $command = qq($echo $ex | $pugs - "Hello Pugs" $redir temp-ex-output);
  diag $command;
  system $command;

  my $expected = "Hello Pugs";
  my $got      = slurp "temp-ex-output";

  is $got, $expected, "Running a script from stdin works";
}