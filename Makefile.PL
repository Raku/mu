#!/usr/bin/perl

use strict;
use FindBin;
use File::Spec;
use Config;
use inc::Module::Install;

chdir $FindBin::Bin;
my $pugs = "pugs$Config{_exe}";
my $full_pugs = File::Spec->catfile($FindBin::Bin, $pugs);
my $full_t = File::Spec->catdir($FindBin::Bin, 't');

name            ('Perl6-Pugs');
version_from    ('lib/Perl6/Pugs.pm');
abstract_from   ('lib/Perl6/Pugs.pm');
author          ('Autrijus Tang <autrijus@autrijus.org>');
license         ('perl');
install_script  ($pugs);
build_requires  ('Test::More');
makemaker_args  (
    test => { TESTS => join ' ', (glob("t/*.t"), glob("t/*/*.t")) },
    MAN1PODS => {},
);

(`ghc --version` =~ /Glasgow/) or die << '.';
*** Cannot find a runnable 'ghc' from path.
*** Please install GHC from http://haskell.org/ghc/.
.

clean_files("src/*.hi", "src/*.o*", "pugs*", "config.h");

my $has_readline = eval {
    require Term::ReadLine;
    require Term::ReadLine::Gnu;
    1
};

if (!$has_readline) {
    warn << '.';
*** Readline support disabled.  If you want readline support,
    please install Term::ReadLine::Gnu from CPAN, as well as
    the GNU Readline headers and shared library.
.
}


open IN, '< lib/Perl6/Pugs.pm' or die $!;
open FH, '> config.h' or die $!;

while (<IN>) {
    /version (\S+) .*\breleased (.*)\./ or next;
    print FH << ".";

#ifdef VERSION
#undef VERSION
#endif
#define VERSION "$1"

#ifdef DATE
#undef DATE
#endif
#define DATE "$2"

.
    last;
}

if ($^O eq 'MSWin32') {
    print FH "#undef PUGS_HAVE_POSIX\n";
}
else {
    print FH "#define PUGS_HAVE_POSIX 1\n";
}

if ($has_readline) {
    print FH "#define PUGS_HAVE_READLINE 1\n";
}
else {
    print FH "#undef PUGS_HAVE_READLINE\n";
}
close FH;

postamble(<< ".");
$pugs: @{[glob("src/*.hs"), glob("src/Rule/*.hs")]}
	ghc --make -o pugs src/Main.hs -isrc

tags ::
	hasktags -c src
.

WriteAll( sign => 1 );

# FIXUP
open MAKEFILE, '< Makefile' or die $!;
my $makefile = do { local $/; <MAKEFILE> };
$full_pugs =~ s{\\}{\\\\}g; $full_pugs =~ s{'}{\\'}g;
$full_t =~ s{\\}{\\\\}g; $full_t =~ s{'}{\\'}g;
$makefile =~ s/\b(test_harness\(\$\(TEST_VERBOSE\), )/ENV->{HARNESS_PERL} = q*$full_pugs*; ENV->{PERL6LIB} = q*$full_t*; $1/;
$makefile =~ s/("-MExtUtils::Command::MM")/"-Iinc" $1/g;
close MAKEFILE;
open MAKEFILE, '> Makefile' or die $!;
print MAKEFILE $makefile;
close MAKEFILE;
