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

sub nonces () { return (".$*PID." ~ int rand 1000) }
my($in_fn, $out_fn) = <temp-ex-input temp-ext-output> >>~<< nonces;
my $h = open(">$in_fn");
$h.print($str);
$h.close();

for @examples -> $ex {
  my $command = "$pugs $ex $redir_in $in_fn $redir_out $out_fn";
  diag $command;
  system $command;

  my $expected = $str;
  my $got      = slurp $out_fn;
  unlink $out_fn;

  is $got, $expected, "-n -e print works like cat";
}

unlink $in_fn;
