#!/usr/bin/pugs

use v6;
use Test;

=pod

Test evaluation of multiple C<-e> switches.

Multiple C<-e> switches are supposed to work just
like C<join "\n"> concatenation .

=cut

my @examples = (
 '-e print -e qq.Hello -e Pugs.',
 '-e print -we qq.Hello -e Pugs.',
 '-e print -wle qq.Hello -e Pugs.',
 '-e print -weqq.Hello -e Pugs.',
 '-e print -e qq.Hel. -e ";print" -e qq.lo. -e ";print" -e "qq.\nPugs."',
 '-e print -e qq.Hel. -w -e ";print" -e qq.lo. -w -e ";print" -e "qq.\nPugs."',
);

plan +@examples +1;

diag "Running under $*OS";

my ($pugs,$redir) = ("./pugs", ">");

if($*OS eq any<MSWin32 mingw msys cygwin>) {
  $pugs = 'pugs.exe';
  $redir = '>';
};

sub nonce () { return (".$*PID." ~ int rand 1000) }
my $out_fn = "temp-ex-output" ~ nonce;

for @examples -> $ex {
  my $command = "$pugs $ex $redir $out_fn";
  diag $command;
  system $command;

  my $expected = "Hello\nPugs";
  my $got      = slurp $out_fn;

  is $got, $expected, "Multiple -e switches work and append the script";
}

my $command = qq($pugs -e @ARGS.perl.say -e "" Hello Pugs $redir $out_fn);
diag $command;
system $command;

my @expected = <Hello Pugs>;
my $got      = slurp $out_fn;
$got .= chomp;
if (substr($got,0,1) ~~ "\\") {
  $got = substr($got,1);
};

my @got      = eval $got;
# fail "FIXME platform specific";
# ??? Worksforme on win32 (CORION)
is @got, @expected, "-e '' does not eat a following argument";

unlink $out_fn;
