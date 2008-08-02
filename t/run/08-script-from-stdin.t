use v6;

use Test;

=begin pod

Test evaluation a script read from STDIN, as
indicated by the C<-> switch.

=end pod

my @examples = map -> Junction $_ { $_.values }, (
   any('say qq.Hello Pugs.',
       'say @*ARGS',
   )
);

plan +@examples;
if $*OS eq "browser" {
  skip_rest "Programs running in browsers don't have access to regular IO.";
  exit;
}

diag "Running under $*OS";

my ($redir,$echo) = (">", "echo");

sub nonce () { return (".{$*PID}." ~ ((1..1000).pick) ~ ".tmp") }
my $tempfile = "temp-ex-output" ~ nonce;
for @examples -> $ex {
  my $command = qq[$echo $ex | $*EXECUTABLE_NAME - "Hello Pugs" $redir $tempfile];
  diag $command;
  system $command;

  my $expected = "Hello Pugs\n";
  my $got      = slurp $tempfile;

  is $got, $expected, "Running a script from stdin works";
  unlink $tempfile;
}
