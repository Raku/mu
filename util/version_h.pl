#!/usr/bin/perl -w
use strict;
use warnings;
use Cwd;

my $version_h = shift || Cwd::cwd() . "/src/Pugs/pugs_version.h";
my $base = shift || Cwd::cwd();
my $svn_entries = "$base/.svn/entries";

my $old_revision = -1;
open IN, "< $version_h" and do {
  while (<IN>) {
    /#define PUGS_SVN_REVISION (\d+)/ or next;
    $old_revision = $1;
    last;
  }
  close IN;
};

my $revision = 0;

# SVK tries to ask the user questions when it has a STDIN and there is
# no repository.  Since we don't need a STDIN anyway, get rid of it.
close STDIN;

if (-r $svn_entries) {
    print "Writing version from $svn_entries to $version_h\n";
    open FH, $svn_entries or die $!;
    while (<FH>) {
        /^ *committed-rev=.(\d+)./ or next;
	$revision = $1;
	last;
    }
    close FH;
} elsif (my @info = qx/svk info/ and $? == 0) {
    print "Writing version from `svk info` to $version_h\n";
    my ($line) = grep /(?:file|svn|https?)\b/, @info;
    ($revision) = $line =~ / (\d+)$/;
}
$revision ||= 0;

if ($revision != $old_revision) {
  open OUT, "> $version_h" or die $!;
  print OUT "#undef PUGS_SVN_REVISION\n";
  print OUT "#define PUGS_SVN_REVISION $revision\n";
  close OUT;

  if ($revision != 0) {
    # rebuild Help.hs to show new revision number
    unlink "$base/src/Pugs/Help.hi";
    unlink "$base/src/Pugs/Help.o";
    exit;
  }
} else {
  print "Not writing $version_h because $old_revision == $revision\n";
}
