#!/usr/bin/perl
use strict;
use warnings;
use Config;
use File::Spec;
use lib 'lib';
use inc::Module::Install;

my $pugs = "pugs$Config{_exe}";
my $version_h = "src/pugs_version.h";
my $config_h = "src/pugs_config.h";
my @srcdirs = ("src", grep(-d, glob "src/*"), grep(-d, glob "src/*/*"));

deny_cygwin     ();

name            ('Perl6-Pugs');
version_from    ('lib/Perl6/Pugs.pm');
abstract_from   ('lib/Perl6/Pugs.pm');
author          ('Autrijus Tang <autrijus@autrijus.org>');
license         ('perl');
install_script  ($pugs);
install_script  (glob('script/*'));
recommends      ('Perl6::Bible');
build_requires  ('ExtUtils::MakeMaker' => 6.15);
include         ('Module::Install::Makefile::Name');
include         ('Module::Install::Makefile::Version');
build_subdirs   (map fixpaths($_), grep {
                   -f "$_/Makefile.PL" && not -l "$_/Makefile.PL"
                 } glob("ext/*")
                );

my $version = version();
$version =~ s{6\.(\d{3})(\d{3})}{join '.', 6, int($1), int($2)}e;
version($version);

makemaker_args  (
    test => { TESTS => join ' ', "t/*/*.t", "t/*/*/*.t" },
    MAN1PODS => {},
);
clean_files     (map fixpaths($_),
    "pugs*", "src/pugs_config.h", $version_h, $config_h,
    'src/Config.hs', 'blib6',
    'test.log',
    map {("$_/*.hi", "$_/*.o*")} @srcdirs
);

set_postamble   ();
no_index        (directory => 'inc', 'examples');
sign            (1);
WritePugs       (5);

################################################################################
sub set_postamble {
    my @srcfiles = grep !/MainCC/, map glob("$_/*.*hs"), @srcdirs;

    my ($ghc, $ghc_version, $ghc_flags) = assert_ghc();
    my $ghc_output = "-o pugs$Config{_exe} src/Main.hs";

    my $pcre = "src/pcre/pcre.o";
    my $pcre_c = "src/pcre/pcre.c";
    my $unicode = "src/UnicodeC.o";
    my $unicode_c = "src/UnicodeC.c";

    my @prereqs = ($config_h, $pcre, $unicode);
    $ghc_output .= " $pcre $unicode";

    my $config = get_pugs_config();
    my $is_win32 = ($^O =~ /MSWin|mingw|cygwin/i);
    my $threaded = (!$is_win32 and try_compile("main = return ()", "-threaded"))
        ? '-threaded' : '';

    postamble(fixpaths(<< "."));
$config_h : lib/Perl6/Pugs.pm util/config_h.pl
	\$(PERL) util/config_h.pl

$version_h : .svn/entries util/version_h.pl
	\$(PERL) util/version_h.pl

.svn/entries :
	\$(NOOP)

$pcre : $pcre_c
	$ghc $threaded -no-link -no-hs-main -O -o $pcre $pcre_c

$unicode : $unicode_c
	$ghc $threaded -no-link -no-hs-main -O -o $unicode $unicode_c

src/Config.hs : util/PugsConfig.pm
	\$(PERL) -Iutil -MPugsConfig -e "PugsConfig->write_config_module" > src/Config.hs

profiled :: src/Config.hs @srcfiles $version_h @prereqs
	$ghc $threaded -auto-all -prof --make $ghc_flags $ghc_output

optimized :: src/Config.hs @srcfiles $version_h @prereqs
	$ghc $threaded -O1 --make $ghc_flags $ghc_output

unoptimized :: src/Config.hs @srcfiles $version_h @prereqs
	$ghc $threaded -O0 --make $ghc_flags $ghc_output

$pugs : src/Config.hs @srcfiles $version_h @prereqs
	$ghc $threaded --make $ghc_flags $ghc_output

ghci ::
	$ghc --interactive $ghc_flags $ghc_output

tags : @srcfiles
	hasktags -c @srcfiles
	sort tags > tags.tmp
	mv tags.tmp tags

INST6_ARCHLIB = blib6/arch
INST6_SCRIPT = blib6/script
INST6_BIN = blib6/bin
INST6_LIB = blib6/lib
INST6_MAN1DIR = blib6/man1
INST6_MAN3DIR = blib6/man3

pure_all ::
	\$(PERLRUN) util/src_to_blib.pl

pure_site_install ::
	\$(NOECHO) \$(MOD_INSTALL) \\
		\$(INST6_LIB) \$(DESTDIR)$config->{privlib} \\
		\$(INST6_ARCHLIB) \$(DESTDIR)$config->{archlib} \\
		\$(INST6_BIN) \$(DESTDIR)$config->{installbin} \\
		\$(INST6_SCRIPT) \$(DESTDIR)$config->{installscript} \\
		\$(INST6_MAN1DIR) \$(DESTINSTALLMAN1DIR) \\
		\$(INST6_MAN3DIR) \$(DESTINSTALLMAN3DIR)
.
}

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
