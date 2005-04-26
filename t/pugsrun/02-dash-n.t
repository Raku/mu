#!/usr/bin/pugs

use v6;
use Test;

=pod

Test -n implementation

The -n command line switch mimics the Perl5 -n command line
switch, and wraps the whole script in

  while (=<>) {
    ...
  };

=cut

my @examples = (
  '-n -e say',
  '-ne say',
  '-e "" -ne say',
);

plan +@examples;

diag "Running under $*OS";

my ($pugs,$redir_in,$redir_out) = ("./pugs", "<", ">");

if($*OS eq any<MSWin32 mingw msys cygwin>) {
  $pugs = 'pugs.exe';
};

my $str = "
foo
bar
";

my $h = open(">temp-ex-input");
$h.print($str);
$h.close();

for @examples -> $ex {
  my $command = "$pugs $ex $redir_in temp-ex-input $redir_out temp-ex-output";
  diag $command;
  system $command;

  my $expected = $str;
  my $got      = slurp "temp-ex-output";
  unlink "temp-ex-output";

  is $got, $expected, "-n -e print works like cat";
}

unlink "temp-ex-input";
