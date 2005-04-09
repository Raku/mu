#!/usr/bin/pugs

use v6;
require Test;

=pod

Test C<-p> implementation

The C<-p> command line switch mimics the Perl5 C<-p> command line
switch, and wraps the whole script in

  while ($_ = =<>) {
    chomp;
    ...         # your script
    say;
  };

=cut

my @examples = (
  '-p',
  '-p "-e1;"',
  '-pe ";"',
  '-pe ""',
  '-p "-e1;" "-e1;"',
  '"-e1;" -p "-e1;"',
);

plan +@examples;

diag "Running under $?OS";

my ($pugs,$redir_in,$redir_out) = ("./pugs", "<", ">");

if($?OS eq any<MSWin32 mingw cygwin>) {
  $pugs = 'pugs.exe';
};

my $str = "
foo
bar
";

my $h = open '>temp-ex-input';
$h.print($str);
$h.close();

for @examples -> $ex {
  my $command = "$pugs $ex $redir_in temp-ex-input $redir_out temp-ex-output";
  diag $command;
  system $command;

  my $expected = $str;
  my $got      = slurp "temp-ex-output";
  unlink "temp-ex-output";

  is $got, $expected, "$ex works like cat";
}

unlink "temp-ex-input";
