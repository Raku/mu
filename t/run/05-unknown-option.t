use v6;

use Test;

=begin pod

Test rejection of unknown command line switches.

Pugs should output

  Unrecognized switch: -foo  (-h will show valid options).

if called with the (unknown) option C<-foo>

=end pod

my @examples = map -> Junction $_ { $_.values },
               map -> Junction $_ { $_.values }, (
    any('-foo ', '-e "print" -foo ', '-c -foo ', '-eprint -foo ')
  ~ any("", '-e "print" ', '-c '),
);

plan +@examples;
if $*OS eq "browser" {
  skip_rest "Programs running in browsers don't have access to regular IO.";
  exit;
}

diag "Running under $*OS";

# Win9x breakage:
my $redir = "2>&1 >";

sub nonce () { return (".{$*PID}." ~ (1..1000).pick) }

for @examples -> $ex {
  my $out_fn = "temp-ex-output" ~ nonce;
  my $command = "$*EXECUTABLE $ex $redir $out_fn";
  diag $command;
  run $command;

  my $expected = "Unrecognized switch: -foo  (-h will show valid options).\n";
  my $got      = slurp $out_fn;
  unlink $out_fn;

  is $got, $expected, "$ex works", :todo;
}
