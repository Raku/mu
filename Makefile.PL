#!/usr/bin/perl

use strict;
use FindBin;
use File::Spec;
use Config;
use inc::Module::Install;

chdir $FindBin::Bin; # XXX Why is this here?

my $pugs = "pugs$Config{_exe}";
my $version_h = nativize("src/pugs_version.h");
my @srcdirs = ("src", map nativize($_), grep(-d, glob "src/*"));

deny_cygwin     ();

name            ('Perl6-Pugs');
version_from    ('lib/Perl6/Pugs.pm');
abstract_from   ('lib/Perl6/Pugs.pm');
author          ('Autrijus Tang <autrijus@autrijus.org>');
license         ('perl');
install_script  ($pugs);
install_script  (glob('script/*'));
build_requires  ('Test::More');
include         ('Module::Install::Makefile::Name');
include         ('Module::Install::Makefile::Version');
build_subdirs   (map nativize($_), grep -e "$_/Makefile.PL", glob("ext/*"));
makemaker_args  (
    test => { TESTS => join ' ', (glob("t/*.t"), glob("t/*/*.t")) },
    MAN1PODS => {},
);
clean_files     (map nativize($_),
    "pugs*", "src/pugs_config.h", "src/pugs_version.h", 
    'src/Config.hs', 'blib6',
    map {("$_/*.hi", "$_/*.o*")} @srcdirs
);

setup_perl6_install();
svn_revision    ();
assert_ghc      ();
create_config_h ();
set_postamble   ();
no_index        (directory => 'inc', 'examples');
WritePugs       (sign => 1);

################################################################################
# XXX Try to be tidy!
# Pretty stuff goes above. Ugly stuff goes below.
# Maybe we can refactor this ugly stuff into a Module::Install::Plugin.

sub svn_revision {
    my $svn_entries = File::Spec->catfile(".svn", "entries");
    if ("@ARGV" eq "-version") {
        open OUT, "> $version_h" or die $!;
        print OUT "#undef PUGS_SVN_REVISION\n";
        if (-r $svn_entries) {
            open FH, $svn_entries;
            while (<FH>) {
                /^ *committed-rev=.(\d+)./ or next;
                print OUT "#define PUGS_SVN_REVISION $1\n";
                # rebuild Help.hs to show new revision number
                unlink "src/Help.hi"; 
                exit;
            }
        }
        print OUT "#define PUGS_SVN_REVISION 0\n";
        exit;
    }

    if (-e $svn_entries) {
        postamble(<< ".");
$version_h : $svn_entries
	\$(PERLRUN) Makefile.PL -version

.
    }
    else {
        open FH, "> $version_h" or die $!;
        print FH "#undef PUGS_SVN_REVISION\n";
        print FH "#define PUGS_SVN_REVISION 0\n";
        close FH;
    }

}

sub create_config_h {
    open IN, '< lib/Perl6/Pugs.pm' or die $!;
    open FH, '> src/pugs_config.h' or die $!;

    while (<IN>) {
        /version (\S+) .*\breleased (.*)\./ or next;
        print FH << ".";

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

    if ($^O eq 'MSWin32') {
        print FH "#undef PUGS_HAVE_POSIX\n";
    }
    else {
        print FH "#define PUGS_HAVE_POSIX 1\n";
    }

    my $has_readline = eval {
        require Term::ReadLine;
        require Term::ReadLine::Gnu;
        1;
    };

    if ($has_readline) {
        print FH "#define PUGS_HAVE_READLINE 1\n";
    }
    else {
        print FH "#undef PUGS_HAVE_READLINE\n";
        warn << '.';
*** Readline support disabled.  If you want readline support,
    please install Term::ReadLine::Gnu from CPAN, as well as
    the GNU Readline headers and shared library.
.
    }
    close FH;
}

sub set_postamble {
    my @srcfiles = map glob("$_/*.*hs"), @srcdirs;

    # turning off unused imports and deprecations for GHC6.4.
	my $ghc_flags = "-isrc -static -Wall -fno-warn-missing-signatures -fno-warn-name-shadowing";
	if (assert_ghc() ge '6.4') {
	  $ghc_flags .= " -fno-warn-deprecations -fno-warn-orphans";
	}

    postamble(nativize(<< "."));
src/Config.hs : util/PugsConfig.pm
	$Config{perlpath} -Iutil -MPugsConfig -e "PugsConfig->write_config_module" > src/Config.hs

profiled :: src/Config.hs @srcfiles $version_h
	ghc -auto-all -prof --make $ghc_flags -o pugs src/Main.hs

optimized :: src/Config.hs @srcfiles $version_h
	ghc -O2 --make $ghc_flags -o pugs src/Main.hs

$pugs : src/Config.hs @srcfiles $version_h
	ghc --make $ghc_flags -o pugs src/Main.hs

tags :
	hasktags -c src

INST6_ARCHLIB = ${\ File::Spec->catfile('blib6', 'arch')}
INST6_SCRIPT = ${\ File::Spec->catfile('blib6', 'script')}
INST6_BIN = ${\ File::Spec->catfile('blib6', 'bin')}
INST6_LIB = ${\ File::Spec->catfile('blib6', 'lib')}
INST6_MAN1DIR = ${\ File::Spec->catfile('blib6', 'man1')}
INST6_MAN3DIR = ${\ File::Spec->catfile('blib6', 'man3')}

pure_site_install ::
	\$(NOECHO) \$(MOD_INSTALL) \\
		\$(INST6_LIB) \$(DESTINSTALLPRIVLIB) \\
		\$(INST6_ARCHLIB) \$(DESTINSTALLARCHLIB) \\
		\$(INST6_BIN) \$(DESTINSTALLBIN) \\
		\$(INST6_SCRIPT) \$(DESTINSTALLSCRIPT) \\
		\$(INST6_MAN1DIR) \$(DESTINSTALLMAN1DIR) \\
		\$(INST6_MAN3DIR) \$(DESTINSTALLMAN3DIR)
.
}
