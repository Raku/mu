#!/usr/bin/perl -w
use strict;
use warnings;
use Cwd;
use File::Spec;

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

if ($ENV{PUGS_EMBED} and $ENV{PUGS_EMBED} =~ /perl5/i) {
    print OUT "#define PUGS_EMBED_PERL5 1\n";
}
else {
    print OUT "#undef PUGS_EMBED_PERL5\n";
    warn << '.';

*** Perl5 embedding disabled.  If you want Perl5 support, please set the
    PUGS_EMBED environment variable to contain "perl5".

.
}

if ($^O =~ /MSWin32|mingw|msys/i) {
    print OUT "#undef PUGS_HAVE_POSIX\n";
}
else {
    print OUT "#define PUGS_HAVE_POSIX 1\n";
}

my $has_readline = try_compile(<< '.');
import System.Console.Readline
main = readline "" >> return ()
.

if ($has_readline) {
    print OUT "#define PUGS_HAVE_READLINE 1\n";
}
else {
    print OUT "#undef PUGS_HAVE_READLINE\n";
    warn << '.';

*** Readline support disabled.  If you want readline support,
    please install the GNU readline library.

.
}

my $has_th = try_compile(<< '.');
{-# OPTIONS_GHC -fth #-}
main = $([| return () |])
.

if ($has_th) {
    print OUT "#define PUGS_HAVE_TH 1\n";
}
else {
    print OUT "#undef PUGS_HAVE_TH\n";
    warn << '.';

*** Template Haskell compiler backends disabled.  If you want
    Template Haskell support, please compile your GHC with the
    GHCi option.

.
}

close OUT;

sub try_compile {
    my $code = shift;
    my $temp = File::Spec->catfile(File::Spec->tmpdir, "pugs-tmp-$$");

    eval {
        open TMP, "> $temp.hs";
        print TMP $code;
        close TMP;
        system(
            ($ENV{GHC} || 'ghc'), @_,
            "--make", "-v0",
            -o => "$temp.exe",
            "$temp.hs"
        );
        
    };

    my $ok = -e "$temp.exe";
    unlink("$temp.exe");
    unlink("$temp.hs");
    unlink("$temp.hi");
    unlink("$temp.o");
    return $ok;
}

