#!/usr/bin/pugs

use v6;
use Test;

=pod

Test rejection of unknown command line switches.

Pugs should output

  Unrecognized switch: -foo  (-h will show valid options).

if called with the (unknown) option C<-foo>

=cut

my @examples = map -> Junction $_ { $_.values }
               map -> Junction $_ { $_.values } (
    any('-foo ', '-e "print" -foo ', '-c -foo ', '-eprint -foo ')
  ~ any("", '-e "print" ', '-c '),
);

plan +@examples;

diag "Running under $*OS";

# Win9x breakage:
my ($pugs,$redir) = ("./pugs", "2>&1 >");

if($*OS eq any<MSWin32 mingw msys cygwin>) {
  $pugs = 'pugs.exe';
};

sub nonces () { return (".$*PID." ~ int rand 1000) }

for @examples -> $ex {
  my $out_fn = "temp-ex-output" ~ nonces;
  my $command = "$pugs $ex $redir $out_fn";
  diag $command;
  system $command;

  my $expected = "Unrecognized switch: -foo  (-h will show valid options).\n";
  my $got      = slurp $out_fn;
  unlink $out_fn;

  is $got, $expected, "$ex works", :todo;
}
