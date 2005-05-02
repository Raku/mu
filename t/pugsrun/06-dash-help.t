#!/usr/bin/pugs

use v6;
use Test;

=pod

Test that the C<--help> command in its various incantations
works.

=cut

my @examples = any<-h --help>;
@examples = map -> Junction $_ { $_.values } 
            map -> Junction $_ { $_, "-w $_", "$_ -w", "-w $_ -w" }
            @examples;

plan +@examples;

diag "Running under $*OS";

my ($pugs,$redir) = ("./pugs", ">");

if($*OS eq any<MSWin32 mingw msys cygwin>) {
  $pugs = 'pugs.exe';
  $redir = '>';
};

sub nonces () { return (".$*PID." ~ int rand 1000) }

for @examples -> $ex {
  my $out_fn = "temp-ex-output" ~ nonces;
  my $command = "$pugs $ex $redir $out_fn";
  diag $command;
  system $command;

  my $got      = slurp $out_fn;
  unlink $out_fn;

  like( $got, rx:perl5/^Usage/, "'$ex' displays help");
}
