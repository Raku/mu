#!/usr/bin/perl -w
use strict;
use warnings;
use Cwd;

my $base = shift || Cwd::cwd();

open IN, "< $base/lib/Perl6/Pugs.pm" or die $!;
open OUT, "> $base/src/pugs_config.h" or die $!;

while (<IN>) {
    /version (\S+) .*\breleased (.*)\./ or next;
    print OUT << ".";

#ifdef PUGS_VERSION
#undef PUGS_VERSION
#endif
#define PUGS_VERSION "$1"

#ifdef PUGS_DATE
#undef PUGS_DATE
#endif
#define PUGS_DATE "$2"

.
    last;
}

if ($^O =~ /MSWin32|mingw|msys/i) {
    print OUT "#undef PUGS_HAVE_POSIX\n";
}
else {
    print OUT "#define PUGS_HAVE_POSIX 1\n";
}

my $has_readline = eval {
    require Term::ReadLine;
    require Term::ReadLine::Gnu;
    1;
};

if ($has_readline) {
    print OUT "#define PUGS_HAVE_READLINE 1\n";
}
else {
    print OUT "#undef PUGS_HAVE_READLINE\n";
    warn << '.';
*** Readline support disabled.  If you want readline support,
    please install Term::ReadLine::Gnu from CPAN, as well as
    the GNU Readline headers and shared library.
.
}
close OUT;
