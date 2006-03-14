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
.
    exit 1;
}

for my $orig (glob join $FS, qw<.. DrIFT src *.hs>) {
    (my $real = $orig) =~ s,.*$FS,src${FS}DrIFT${FS},;
    next unless -f $real;
    my $clean = sub { $_[0] = File::Spec->canonpath(File::Spec->rel2abs($_[0])) };

    print $clean->($orig);
    print $clean->($real);

    print "rm $orig\n"                                    if $Conf{verbose};
    do { 1 == unlink  $orig        or die "unlink: $!" }  unless $Conf{no};

    print "ln -s $real $orig\n"                           if $Conf{verbose};
    do { 1 == symlink $real, $orig or die "symlink: $!" } unless $Conf{no};
}

