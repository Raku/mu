#!/usr/bin/perl
use strict;
use warnings;
use Config;
use File::Spec;
use inc::Module::Install;

my $pugs = "pugs$Config{_exe}";
my $version_h = "src/pugs_version.h";
my $config_h = "src/pugs_config.h";
my @srcdirs = ("src", grep(-d, glob "src/*"));

deny_cygwin     ();
my ($ghc, $ghc_version) = assert_ghc();

name            ('Perl6-Pugs');
version_from    ('lib/Perl6/Pugs.pm');
abstract_from   ('lib/Perl6/Pugs.pm');
author          ('Autrijus Tang <autrijus@autrijus.org>');
license         ('perl');
install_script  ($pugs);
install_script  (glob('script/*'));
build_requires  ('ExtUtils::MakeMaker' => 6.15);
include         ('Module::Install::Makefile::Name');
include         ('Module::Install::Makefile::Version');
build_subdirs   (map fixpaths($_), grep -e "$_/Makefile.PL", glob("ext/*"));
makemaker_args  (
    test => { TESTS => join ' ', (glob("t/*.t"), glob("t/*/*.t")) },
    MAN1PODS => {},
);
clean_files     (map fixpaths($_),
    "pugs*", "src/pugs_config.h", $version_h, $config_h,
    'src/Config.hs', 'blib6',
    'test.log',
    map {("$_/*.hi", "$_/*.o*")} @srcdirs
);

setup_perl6_install();
set_postamble   ();
no_index        (directory => 'inc', 'examples');
WritePugs       (sign => 1);

################################################################################
sub set_postamble {
    my @srcfiles = map glob("$_/*.*hs"), @srcdirs;

    # turning off unused imports and deprecations for GHC6.4.
    my $ghc_flags = "-L. -Lsrc -Lsrc/pcre -I. -Isrc -Isrc/pcre -i. -isrc -isrc/pcre -static -Wall -fno-warn-missing-signatures -fno-warn-name-shadowing";
    if ($ghc_version ge '6.4') {
        $ghc_flags .= " -fno-warn-deprecations -fno-warn-orphans";
    }
    my $ghc_output = "-o pugs$Config{_exe} src/Main.hs";
    my @prereqs = ($config_h, qw( src/UnicodeC.c src/UnicodeC.h ));

    my $pcre = "src/pcre/pcre.o";
    my $pcre_c = "src/pcre/pcre.c";

    $ghc_output .= " $pcre";
    push @prereqs, $pcre;

    postamble(fixpaths(<< "."));
$config_h : lib/Perl6/Pugs.pm util/config_h.pl
	\$(PERL) util/config_h.pl

$version_h : .svn/entries util/version_h.pl
	\$(PERL) util/version_h.pl

.svn/entries :
	\$(NOOP)

$pcre : $pcre_c
	$ghc -no-link -no-hs-main -O -o $pcre $pcre_c

src/Config.hs : util/PugsConfig.pm
	\$(PERL) -Iutil -MPugsConfig -e "PugsConfig->write_config_module" > src/Config.hs

profiled :: src/Config.hs @srcfiles $version_h @prereqs
	$ghc -auto-all -prof --make $ghc_flags $ghc_output

optimized :: src/Config.hs @srcfiles $version_h @prereqs
	$ghc -O2 --make $ghc_flags $ghc_output

$pugs : src/Config.hs @srcfiles $version_h @prereqs
	$ghc --make $ghc_flags $ghc_output

tags :
	hasktags -c src

INST6_ARCHLIB = 'blib6/arch'
INST6_SCRIPT = 'blib6/script'
INST6_BIN = 'blib6/bin'
INST6_LIB = 'blib6/lib'
INST6_MAN1DIR = 'blib6/man1'
INST6_MAN3DIR = 'blib6/man3'

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
