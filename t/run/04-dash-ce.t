use v6;

use Test;

=begin pod

Test evaluation of combination of C<-e> and C<-c> switches.

They should all do a syntax check and never evaluate the
C<-e> fragments.

=end pod

my @examples = (
  '-ce "print qq,Code got interpreted!,"',
  '-c -e "print qq,Code got interpreted!,"',
  '-e "print qq,Code got interpreted!," -c',
  '-eprint -c',
  '-ceprint',
);

plan +@examples;
if $*OS eq "browser" {
  skip_rest "Programs running in browsers don't have access to regular IO.";
  exit;
}

diag "Running under $*OS";

my $redir =  ">";

if $*OS eq any <MSWin32 mingw msys cygwin> {
  $redir = '>';
};

sub nonce () { return (".{$*PID}." ~ (1..1000).pick) }

for @examples -> $ex {
  my $out_fn = "temp-ex-output" ~ nonce;
  my $command = "$*EXECUTABLE_NAME $ex $redir $out_fn";
  diag $command;
  run $command;

  my $expected = "-e syntax OK\n";
  my $got      = slurp $out_fn;
  unlink $out_fn;

  is $got, $expected, "$ex works";
}
