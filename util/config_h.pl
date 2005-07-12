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

my ($ghc,$base) = @ARGV;

$ghc ||= $ENV{GHC} || 'ghc';
$base ||= Cwd::cwd();

open IN, "< $base/lib/Perl6-Pugs/lib/Perl6/Pugs.pm" or die $!;
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
    
    my $dir = tempdir( CLEANUP => 1 );
    my $temp = File::Spec->catfile($dir, "compile-test");

    eval {
      open TMP, "> $temp.hs"
        or die "Couldn't create '$temp.hs': $!";
      print TMP $code
        or die "Couldn't write to '$temp.hs': $!";
        
      close TMP
        or die "Couldn't close '$temp.hs': $!";
      
      my $command = join(" ",
              $ghc, @_,
              "--make", "-v0",
              -o => "$temp.exe",
              "$temp.hs");
      system($command) == 0
        or die "Couldn't run '$command': $!";
    };
    if ($@) {
      warn $@;
      return;
    };

    my $ok = -e "$temp.exe";
    for ("$temp.exe", "$temp.hs", "$temp.hi",  "$temp.o") {
      if (-f $_) {
        unlink $_
          or warn "Couldn't remove $_: $!";
      };
    };
    return $ok;
}

