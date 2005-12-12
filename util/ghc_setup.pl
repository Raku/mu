#!/usr/bin/perl
#
# Silly GHC ./Setup helper, simply to strip the DESTDIR away
# when it is not passed in, and add "--copy-prefix" if it is.
#

use Config;
use File::Spec;

my $setup = File::Spec->catfile(File::Spec->curdir, "Setup$Config{_exe}");
my @args = grep !/^--[-\w]*prefix=$/, @ARGV;
my $prompt_register;

if ($args[0] eq 'copy') {
    if (@args != @ARGV and is_root()) {
        # if there's no prefix, we want an install.
        $args[0] = 'install';
    }
    else {
        # If this is merely a copy, tell the user to "make register".
        $prompt_register = 1;
    }
}
print "*** Running: $setup @args\n";

my $rv = system($setup, @args);
if ($rv == 0) {
    if ($prompt_register) {
        print << ".";
*** Installed!  If you would like to make Pugs a system-wide GHC package
    for use with other Haskell programs, type '$Config{make} register'.
.
    }
    else {
        print << ".";
*** Installed successfully!
.
    }
}
else {
    print << ".";
*** Installation failed (exit code $rv).
.
}

exit $rv;

sub is_root {
    local $@;
    eval('$> == 0') || $@; # System with no concepts of UIDs
}
