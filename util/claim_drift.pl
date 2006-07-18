#!/usr/bin/perl -w

use strict;
use Getopt::Long;
use Config;
use File::Spec;

# If you have several working directories and they all use a shared DrIFT,
# you need to point symlinks to the current one if you want to regenerate
# files. We should probably make this arrangement less hacky, but until
# we do, this "claims" DrIFT links.
#
# usage: cd $pugs_wd ; util/claim-drift.pl

our ($FS) = ($Config{sitelib} =~ /([\/\\])/)
       or die "Can't determine file_sep";

GetOptions \our %Conf, qw(--verbose|v --no|n);

if (! -d "..${FS}DrIFT") {
    warn <<".";
*** No DrIFT directory found. Make sure you are in the top level of a pugs
    working directory, and that you have a copy of DrIFT one level up.

    Obtain a copy of DrIFT at:
      <http://repetae.net/~john/computer/haskell/DrIFT/drop/>

    And rename the distro to "DrIFT", e.g.,
      tar xzvf DrIFT-2.1.2.tar.gz
      mv DrIFT-2.1.2 DrIFT
.
    exit 1;
}

for my $real (glob join $FS, qw<src DrIFT *Rule*.hs>) {
    (my $orig = $real) =~ s,.*$FS,..${FS}DrIFT${FS}src${FS},;

    clean($orig);
    clean($real);

    if (-f $orig) {
        print "rm $orig\n"                                if $Conf{verbose};
        do { 1 == unlink  $orig    or die "unlink: $!" }  unless $Conf{no};
    }

    print "ln -s $real $orig\n"                           if $Conf{verbose};
    do { 1 == symlink $real, $orig or die "symlink: $!" } unless $Conf{no};
}

sub clean { $_[0] = File::Spec->canonpath(File::Spec->rel2abs($_[0])) }
