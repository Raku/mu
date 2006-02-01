#!/usr/bin/perl -w
use strict;
use warnings;
use Cwd;
use File::Spec;
use File::Temp qw(tempdir);

=pod

=head1 DOCUMENTATION

This file seems to determine some configuration.

  util/config_h.pl GHC BASEDIR

  GHC     - Location and flags of the ghc executable
              (defaults to 'ghc')
  BASEDIR - Base directory of the Pugs installation 
              (defaults to current directory)

=cut

my ($ghc_line, $base) = @ARGV or exit;

$ghc_line ||= $ENV{GHC} || 'ghc';
$base ||= Cwd::cwd();

my ($ghc, @ghc_args) = split /\s+/, $ghc_line;

open IN, "< $base/lib/Perl6/Pugs.pm" or die $!;
open OUT, "> $base/src/Pugs/pugs_config.h" or die $!;

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

# FIXME: we assume if you have cywin, you're still using ghc-msys
if ($^O =~ /MSWin32|mingw|msys|cygwin/i) {
    print OUT "#undef PUGS_HAVE_POSIX\n";
}
else {
    print OUT "#define PUGS_HAVE_POSIX 1\n";
}

my $has_readline = try_compile(<< '.');
import System.Console.Readline
main :: IO ()
main = readline "" >> return ()
.


my $has_th = try_compile(<< '.');
{-# OPTIONS_GHC -fth #-}
main :: (Monad m) => m ()
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
    my $temp = "pugs-tmp-$$";

    eval {
        open TMP, "> $temp.hs";
        print TMP $code;
        close TMP;
        system(
            $ghc, @ghc_args,
            "--make", "-v0",
            -o => "$temp.exe",
            "$temp.hs"
        );

    };

    my $ok = -s "$temp.exe";

    unlink("$temp.exe");
    unlink("$temp.hs");
    unlink("$temp.hi");
    unlink("$temp.o");

    return $ok;
}

