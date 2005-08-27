#!/usr/bin/perl
# Hack. Use this as HARNESS_PUGS for run-smoke.pl.
# (*Only* for run-smoke.pl, as this script highly depends on the
# exact commandline options given.)
# Searching for a way to keep pugs from loading Test.pm,
#   pugs -MMarkTestPMAsLoaded -CPIL ...normal options here...
# with MarkTestPMAsLoaded.pm containing
#   %*INC<Test.pm> = "<precompiled to JS>";
# should work, but doesn't (pugs dies with "can't open file").

use FindBin;
use File::Spec;
sub pwd { File::Spec->catfile($FindBin::Bin, @_) }

warn "# @ARGV\n";

if($ARGV[1] eq "-w" and $ARGV[2]) {
  local $/;
  open my $fh, "<", $ARGV[2] or die "Couldn't open \"$ARGV[2]\": $!\n";
  my $src = <$fh>;
  $src =~ s/^use Test//gm; # hack

  exec pwd("runjs.pl"), "-e", $src;
} else {
  exec pwd("..", "..", "pugs"), @ARGV[1..$#ARGV];
}
