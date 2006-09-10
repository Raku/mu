#!/usr/bin/perl -w

# TODO: add a comment explaining what this small program does.

use strict;
use warnings;
use FindBin qw($Bin);

chdir_to_base();
my $version_h = shift || "$Bin/../src/Pugs/pugs_version.h";
my $base = shift || "$Bin/../";
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

# We can't use SVN keyword expansion (like $Rev$), because
# that is only updated when the file in which the keyword appears
# is modified.
my $revision = 0;

# SVK tries to ask the user questions when it has a STDIN and there is
# no repository.  Since we don't need a STDIN anyway, get rid of it.
close STDIN;

if (-e "$base/MANIFEST") {
    # This is a release -- do nothing!
}
elsif (-r $svn_entries) {
    print "Writing version from $svn_entries to $version_h\n";
    open FH, $svn_entries or die "Unable to open file ($svn_entries). Aborting. Error returned was: $!";
    while (<FH>) {
        /^ *committed-rev=.(\d+)./ or next;
        $revision = $1;
        last;
    }
    close FH;
} elsif (my @info = qx/svk info/ and $? == 0) {
    print "Writing version from `svk info` to $version_h\n";
    if (my ($line) = grep /(?:file|svn|https?)\b/, @info) {
        ($revision) = $line =~ / (\d+)$/;
    } elsif (my ($source_line) = grep /^(Copied|Merged) From/, @info) {
        if (my ($source_depot) = $source_line =~ /From: (.*?), Rev\. \d+/) {
            $source_depot = '/'.$source_depot; # convert /svk/trunk to //svk/trunk
            if (my @info = qx/svk info $source_depot/ and $? == 0) {
                if (my ($line) = grep /(?:file|svn|https?)\b/, @info) {
                    ($revision) = $line =~ / (\d+)$/;
                }
            }
        }
    }
}
$revision ||= 0;

# WARNING! don't modify the following output, since smartlinks.pl relies on it.
print "Current version is $revision\n";

if ($revision != $old_revision) {
  # As we've closed STDIN (filehandle #0), slot #0 is available for new
  # filehandles again. If we opened a new file ($version_h) without turning
  # "io" warnings off, perl will print "Filehandle STDIN reopened...", because
  # our handle for $version_h got slot #0, like STDIN.
  no warnings "io";
  open OUT, "> $version_h" or die "unable to open file ($version_h) for writing. Aborting. Error was: $!";
  print OUT "#undef PUGS_SVN_REVISION\n";
  print OUT "#define PUGS_SVN_REVISION $revision\n";
  close OUT;

  if ($revision != 0) {
    # rebuild Help.hs to show new revision number
    unlink "$base/dist/build/src/Pugs/Version.hi";
    unlink "$base/dist/build/src/Pugs/Version.o";
    unlink "$base/dist/build/Pugs/Version.hi";
    unlink "$base/dist/build/Pugs/Version.o";
    exit;
  }
} elsif ($revision) {
  print "Not writing $version_h because $old_revision == $revision\n";
}

sub chdir_to_base {
    # rest of script expects us to be at the base dir, so find it
    # and chdir to it
    my $svn_entries = ".svn/entries";
    if (-r $svn_entries) {
        open my $fh, "<", $svn_entries
            or die "Couldn't open $svn_entries: $!\n";
        my ($url, $repo);
        while (<$fh>) {
            if (/^ *url="(.*)"/) {
                $url = $1
            } elsif (/^ *repos="(.*)"/) {
                $repo = $1;
            }
            $url && $repo && last;
        }
        close $fh;
        return unless $url && $repo;
        return if $url eq $repo;
        $url =~ s|$repo/||;
        $url =~ s|[^/]+|..|g;
        chdir $url;
    }
    # XXX Make work for svk too
}
