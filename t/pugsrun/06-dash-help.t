#!/usr/bin/pugs

use v6;
require Test;

=pod

Test that the C<--help> command in its various incantations
works.

=cut

my @examples = ("-h", "--help");
@examples = @examples.map():{ $_, "-w $_", "$_ -w", "-w $_ -w" };

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

  my $got      = slurp "temp-ex-output";
  unlink "temp-ex-output";

  like( $got, rx:perl5/^Usage/, "'$ex' displays help");
}
