#!/usr/bin/pugs

use v6;
require Test;

=pod

Test -n implementation

The -n command line switch mimics the Perl5 -n command line
switch, and wraps the whole script in

  while (<>) {
    ...
  };

=cut

my @examples = (
'-n -e print',
);

plan +@examples;

diag "Running under $?OS";

my ($pugs,$redir_out,$redir_in) = ("./pugs", "<", "&>");

if ($?OS eq "MSWin32") {
  $pugs = 'pugs.exe';
  $redir_out = '>';
  # $redir_in = '<';
};

my $str = <<FOO
foo
bar
FOO

spew ">temp-ex-input", $str;

for @examples -> $ex {
  my $command = "$pugs $ex $redir_in temp-ex-input $redir_out temp-ex-output";
  diag $command;
  system $command;

  my $expected = $str;
  my $got      = slurp "temp-ex-output";
  unlink "temp-ex-output";

  todo_is $expected, $got, "-n -e print works like cat";
}
