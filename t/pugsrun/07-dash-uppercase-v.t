#!/usr/bin/pugs

use v6;
require Test;

=pod

Test handling of C<-V> and C<-V:option>.


=cut

plan 2;

diag "Running under $?OS";

my ($pugs,$redir) = ("./pugs", ">");
if ($?OS eq "MSWin32") {
  $pugs = 'pugs.exe';
};

sub run_pugs ($c) {
  my $tempfile = "temp-ex-output";
  my $command = "$pugs $c $redir $tempfile";
  diag $command;
  system $command;
  my $res = slurp $tempfile;
  unlink $tempfile;
  return $res;
}

my $pugs_config = run_pugs('-V');
ok( ($pugs_config ~~ rx:perl5/^This is Perl6 User's Golfing System/), "Got some config data")
  or diag $pugs_config;

# Generalize this:
$pugs_config = run_pugs('-V:path_sep');

my $local_sep = (eval '%?CONFIG{path_sep}') // '%?CONFIG{path_sep} <undefined>';

todo_is( $pugs_config, $local_sep, "path_sep works" );

#for (%config.keys() -> $entry) {
#  diag $entry;
#  my $result = run_pugs("-V:" ~ %config{$entry});
#  is $result, "$entry: %config{$entry}\n", "Pugs -V:$entry works";
#};

