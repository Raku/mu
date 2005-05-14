#!/usr/bin/pugs

use v6;
use Test;

=pod

Test evaluation a script read from STDIN, as
indicated by the C<-> switch.

=cut

my @examples = map -> Junction $_ { $_.values } (
   any('say qq.Hello Pugs.',
       'say @ARGS',
   )
);

plan +@examples;

diag "Running under $*OS";

my ($pugs,$redir,$echo) = ("./pugs", ">", "echo");

if($*OS eq any<MSWin32 mingw msys cygwin>) {
  $pugs = 'pugs.exe';
};

sub nonces () { return (".$*PID." ~ (int rand 1000) ~ ".tmp") }
my $tempfile = "temp-ex-output" ~ nonces;
for @examples -> $ex {
  my $command = qq($echo $ex | $pugs - "Hello Pugs" $redir $tempfile);
  diag $command;
  system $command;

  my $expected = "Hello Pugs\n";
  my $got      = slurp "temp-ex-output";

  is $got, $expected, "Running a script from stdin works";
  unlink $tempfile;
}
