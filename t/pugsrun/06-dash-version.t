#!/usr/bin/pugs

use v6;
require Test;

=pod

Test that the C<--version> command in its various incantations
works.

=cut

my @examples = ("-v", "--version");
@examples = @examples.map():{ $_, "-w $_", "$_ -w", "-w $_ -w" };

plan +@examples;

diag "Running under $?OS";

my ($pugs,$redir) = ("./pugs", "&>");

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

  my $res = ($got ~~ rx:perl5/Version: 6\.0\.\d+ \(r\d+\)/);
  ok ($res, "'$ex' displays help")
    or diag $got;
}
