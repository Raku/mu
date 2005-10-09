#!/usr/bin/perl
#
# Silly GHC ./Setup helper, simply to strip the DESTDIR away
# when it is not passed in, and add "--copy-prefix" if it is.
#

use Config;
use File::Spec;

my $setup = File::Spec->catfile(File::Spec->curdir, "Setup$Config{_exe}");
my @args = grep !/^--prefix=$/, @ARGV;
if (@args != @ARGV and $args[0] eq 'copy') {
    # if there's no prefix, we want an install.
    $args[0] = 'install';
}
print "*** Running: $setup @args\n";
exit system($setup, @args);
