#!/usr/bin/pugs

use v6;
require Test;

=pod

Test rejection of unknown command line switches.

Pugs should output

  Unrecognized switch: -foo  (-h will show valid options).

if called with the (unknown) option C<-foo>

=cut

my @examples;
push @examples, '-foo';
push @examples, '-e "print" -foo';
push @examples, '-foo -c';

#@examples = (); # unTODOme

plan +@examples;

diag "Running under $?OS";

# Win9x breakage:
my ($pugs,$redir) = ("./pugs", "2>&1 >");

if($?OS eq any<MSWin32 mingw cygwin>) {
  $pugs = 'pugs.exe';
};

for @examples -> $ex {
  my $command = "$pugs $ex $redir temp-ex-output";
  diag $command;
  system $command;

  my $expected = "Unrecognized switch: -foo  (-h will show valid options).\n";
  my $got      = slurp "temp-ex-output";
  unlink "temp-ex-output";

  todo_is $got, $expected, "$ex works";
}
