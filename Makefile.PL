#!/usr/bin/perl
use strict;
use warnings;
use Config;
use Cwd qw(abs_path);
use File::Spec;
use FindBin;
BEGIN { chdir $FindBin::RealBin };
use inc::Module::Install;
use lib 'inc';
use PugsBuild::Config;

my @warn;
my $pugs = "pugs$Config{_exe}";
my ($fs) = ($Config{sitelib} =~ /([\/\\])/)
       or die "Can't determine file_sep";
my $thispugs = ".${fs}pugs$Config{_exe}";
my $version_h = "src/Pugs/pugs_version.h";
my $config_h = "src/Pugs/pugs_config.h";
my @srcdirs  = grep {-d} glob("src"), glob("src/*"), glob("src/*/*"), glob("src/*/*/*");
my @hsfiles  = map {glob "$_/*.hs"} @srcdirs;
push @hsfiles, qw<src/Pugs/Config.hs src/Pugs/CodeGen/PIR/Prelude.hs>;
my @hppfiles = map {my $x=$_; $x=~s/\.hs$/.hpp/; $x} @hsfiles;

warn_cygwin     ();

print "*** Probing configuration (please ignore any warnings)...\n\n";

name            ('Perl6-Pugs');
version_from    ('lib/Perl6/Pugs.pm');
abstract_from   ('lib/Perl6/Pugs.pm');
author          ('Autrijus Tang <autrijus@autrijus.org>');
license         ('perl');
install_script  ($pugs);
install_script  (glob('script/*'));
recommends      ('Perl6::Bible');
recommends      ('Inline');
recommends      ('Filter::Simple');
build_requires  ('ExtUtils::MakeMaker' => 6.15);
include         ('Module::Install::Makefile::Name');
include         ('Module::Install::Makefile::Version');
build_subdirs   (map fixpaths($_), grep {
                   -f "$_/Makefile.PL" && not -l "$_/Makefile.PL"
                 } glob("ext/*")
                );

my $version = version();
$version .= 0 until length($version) >= length('0.123456');
$version =~ s{6\.(\d{3})(\d{3})?}{join '.', 6, int($1), int($2||0)}e;
version($version);

makemaker_args  (
    test => { TESTS => join ' ', "t/*/*.t", "t/*/*/*.t" },
    MAN1PODS => {},
);

if (my $prefix = PugsBuild::Config->lookup('install_dir')) {
    makemaker_args(PREFIX => $prefix);
}

clean_files     (map fixpaths($_),
    "pugs*", "src/Pugs/pugs_config.h", $version_h, $config_h,
    'src/Pugs/Config.hs', 'blib6', 'dist',
    'src/Pugs/PreludePC.hs',
    'src/Pugs/CodeGen/PIR/Prelude.hs',
    'src/Pugs/CodeGen/PIR/Prelude.hi',
    'src/Pugs/CodeGen/PIR/Prelude.o',
    "src/gen_prelude$Config{_exe}",
    'test.log',
    'src/Pugs/Embed/Parrot.hs',
    'src/Pugs/Embed/Parrot_hsc.*',
    'src/Pugs/Run/Perl5_stub.*',
    'src/Data/Yaml/Syck_stub.*',
    'src/Data/Yaml/Syck.hs',
    (map {("$_/*.o*")} qw(pcre perl5 syck cbits)),
    (map {("$_/*.hpp", "$_/*.hi", "$_/*.o*")} @srcdirs),
    'temp-ex*',
    'unlink-test*',
    'tmp-Prelude.pm',
	'Pugs.cabal',
	'Setup.hi',
	'Setup',
	'.setup-config'
);

set_postamble   ();
no_index        (
    directory =>
        qw< inc debian modules perl5 ext script util docs examples src >
);
sign            (1);
WritePugs       (5);

print(('=' x 78), "\n\n");

print @warn;

print << ".";
*** Enter '$Config{make}' to build Pugs.  If compilation is too slow,
    consider using '$Config{make} unoptimized' instead.

.

################################################################################
sub set_postamble {
    my @srcfiles = map { glob("$_/*.*hs") } @srcdirs;
    push @srcfiles, map { glob("$_/*.*hs-boot") } @srcdirs;
    push @srcfiles, map { map { substr($_, 0, -1) } glob("$_/*.*hsc") } @srcdirs;

    my ($ghc, $ghc_version, $ghc_flags) = assert_ghc();
    my $hsc2hs = $ENV{HSC2HS};
    my $setup = File::Spec->catfile(".", "Setup$Config{_exe}");

    if (!$hsc2hs) {
        $hsc2hs = $ghc;
        $hsc2hs =~ s{(.*)ghc}{$1hsc2hs};
    }

    system($ghc, '--make', '-o' => $setup, 'Setup.lhs');

    my $heap = PugsBuild::Config->lookup('ghc_heap_size');
    $ghc_flags .= " +RTS -M$heap -RTS" if $heap;
    # $ghc_flags .= ' -dcore-lint';
    # $ghc_flags .= " -keep-tmp-files";
    if (has_ghc_package('plugins')
        and  try_compile("import System.Eval\n"
                        ."main :: IO ()\n"
                        .'main = (eval_ "return ()" [] [] [] [] :: IO (Either [String] (Maybe ()))) >> return ()', '-package', 'plugins')) {
        $ghc_flags .= ' -package plugins -DPUGS_HAVE_HSPLUGINS=1 ';
    } else {
        push @warn, << '.';
*** Inline Haskell support disabled.  If you want dynamic loading
    of haskell modules, please install the hs-plugins library:
        http://www.cse.unsw.edu.au/~dons/code/hs-plugins
    Remember to "make register" after "make install" for hs-plugins!

.
    }

    if (has_ghc_package('readline') 
        and  try_compile("import System.Console.Readline\n"
                        ."main :: IO ()\n"
                        .'main = readline "" >> return ()')) {
      $ghc_flags .= ' -DPUGS_HAVE_READLINE=1 -package readline';
    }
    else {
      push @warn, << '.';
*** Readline support disabled.  If you want readline support,
    please install the GNU readline library.

.
    }

    my $ghc_output = ''; # "-o pugs$Config{_exe} src/Main.hs";
    my $hasktags = $ENV{HASKTAGS} || 'hasktags';

    my $pcre_c = "src/pcre/pcre.c";
    my @cbits_c = (glob("src/syck/*.c"), glob("src/cbits/*.c"));
    my $pcre = "src/pcre/pcre.o";
    my @cbits = map { substr($_, 0, -1) . 'o' } @cbits_c;
    my $unicode = "src/UnicodeC.o";
    my $unicode_c = "src/UnicodeC.c";

    my @prereqs = ($pcre, @cbits, $unicode);
    #$ghc_output .= " $pcre @cbits $unicode";

    my $embed_flags = "";
    my $hsc2hs_flags = "";
    my $ccdlflags = "";

    if ($ENV{PUGS_EMBED} and $ENV{PUGS_EMBED} =~ /\bperl5\b/i  and !($Config{cc} eq 'cl') )  {
        push @prereqs, "src/perl5/perl5.o"; # XXX
        # $ghc_output .= " src/perl5/perl5.o ";
        $ghc_flags .= " -DPUGS_HAVE_PERL5 ";
        my $flags = "$Config{ccflags} $Config{ccdlflags}";
        if ($flags =~ /\S/) {
            $flags =~ s{([\\"'])}{\\$1}g;
            $ccdlflags .= (/^-D/ ? ' -optc' : ' -optl') . qq["$_" ]
              for split /\s+/, $flags;
        }
    }
    elsif( $ENV{PUGS_EMBED} and $Config{cc} eq 'cl' and $^O =~ /Win32/ )   {
        $ENV{PUGS_EMBED} =~ s/\bperl5\b//g;
        push @warn, << '.'
*** Perl 5 embedding disabled on Win32 systems.
.
    }
    else {
        push @warn, << '.';
*** Perl 5 embedding disabled.  If you want Perl 5 support, please set the
    PUGS_EMBED environment variable to contain "perl5".

.
    }
    
    if ($ENV{PUGS_EMBED} and $ENV{PUGS_EMBED} =~ /\bparrot\b/i and $Config{cc} eq 'cl') {
        push @warn, << '.';
*** Parrot linking not supported with MSVC.  Parrot linking will be disabled.

.
        $ENV{PUGS_EMBED} =~ s/\bparrot\b//g;
    }

    # Respect CC setting
    my $cc = $ENV{CC} || 'gcc';
    $ghc_flags .= " -pgmc $cc " unless $cc eq 'gcc';

    if ($ghc_version =~ /^6.4(?:.0)?$/) {
        $ghc_flags .= " -DWITH_UNICODEC ";
    }
    else {
        $unicode = $unicode_c = '';
    }

    # GHC 6.4.1, OS X / Tiger and gcc4.0 doesn't work together. complain.
    if ($Config{osname} eq "darwin") {
      my $gcc_version = `$cc -v 2>&1`;
      my $os_version = `uname -r`;
      if ($ghc_version =~ /^6.4(?:.0)?$/ and $gcc_version =~ /\b4.0.0\b/ && $os_version =~ /^8./) {
         die <<EOD;
*** GHC 6.4.0 cannot work with GCC 4.0 on Mac OS X.
    Please upgrade to GHC 6.4.1, or use "sudo gcc_select 3.3" to pick an
    earlier version of gcc, or set the CC environment variable to
    "/usr/bin/gcc-3.3".
EOD
      }
    }

    if ($Config{cf_by} eq 'Debian Project' and $ENV{PUGS_EMBED} and $ENV{PUGS_EMBED} =~ /\bperl5\b/i) {
        # Is it safe to remove 'Debian Project' above, to test on all platforms?
        my $need_path = 1;
        my $libperlpath = '';
        my $libperl = $Config{libperl};
        foreach my $path (split(/\s/, $Config{libpth} . ' ' . $Config{libsdirs})) {
            if (-e "$path/libperl.so") {
                $need_path = 0;
                last;
            } elsif (-e "$path/$libperl") {
                $libperlpath ||= $path;
            }
        }
        if ($need_path) {
            my $message = '';
            $message .= qq[        * Symlink $libperlpath/$libperl to libperl.so\n] if ($libperlpath and $libperl);
            $message .= qq[        * Install libperl-dev package\n] if ($Config{cf_by} eq 'Debian Project');
            die <<EOD;
*** Could not find libperl.so in: $Config{libpth} $Config{libsdirs}
    Solutions include:
        * Remove perl5 from PUGS_EMBED
$message
EOD
        }
    }

    if ($ENV{PUGS_EMBED} and $ENV{PUGS_EMBED} =~ /\bparrot\b/i) {
        my $base = $ENV{PARROT_PATH};
        if (!$base and -d "../parrot") {
            $base = abs_path('../parrot/');
        }
        (-d $base and -e "$base/parrot-config.imc")
            or die "*** Please set \$ENV{PARROT_PATH} to the base path with a built parrot tree.\n";
        my $ldflags = parrot_config($base, 'ldflags');
        my $libs = parrot_config($base, 'libs');
        my $icuflags = parrot_config($base, 'icu_shared');

        # strip non-GHC flags
        $ldflags =~ s/-[^IlL]\S*//g;
        $libs =~ s/-[^IlL]\S*//g;
        $icuflags =~ s/-[^IlL]\S*//g;

        $embed_flags .= " -I$base/include -L$base/blib/lib -DPUGS_HAVE_PARROT=1 -L$base/blib/lib -L/usr/local/lib $ldflags ";
        $ghc_output .= " -lparrot $libs $icuflags ";
        my $config = "$base/src/parrot_config$Config{_o}";
        $ghc_output .= " $config " if -e $config;

        # parrot include paths for hsc2hs
        $hsc2hs_flags .= " -DPUGS_HAVE_PARROT -I$base/include ";
    }
    else {
        push @warn, << '.';
*** Parrot linking disabled; external 'parrot' executable will be used for
    Rules support -- please make sure that the executable is available in
    your PATH.  If you want to link against Parrot, set the PUGS_EMBED
    environment variable to contain 'parrot', the PARROT_PATH environment
    variable to the path of a built parrot tree, then run Makefile.PL again.

.
    }

    $hsc2hs_flags .= " -Isrc/syck "; # for Data.Yaml

    my $config = get_pugs_config();
    my $is_win32 = ($^O =~ /MSWin|mingw|cygwin/i);
    my $threaded = (!$is_win32 and try_compile("main :: IO ()\nmain = return ()", "-threaded"))
        ? '-threaded' : '';

    if ($threaded and $ENV{PUGS_EMBED} and $ENV{PUGS_EMBED} =~ /\bperl5\b/ and !$Config{usethreads}) {
        push @warn, << '.';
*** Thread support disabled because you are building with embedded perl5 and
    your perl5 is not threaded.

.

        $threaded = '';
    }

    # XXX - hsplugins doesn't build with profiles
    my $profiled_flags = $ghc_flags;
    $profiled_flags =~ s{-DPUGS_HAVE_HSPLUGINS=1}{};
    $profiled_flags =~ s{-package plugins}{};

    my $emit = sub {
        my $c = shift;
        my $o = substr($c, 0, -1) . 'o';
        return "$o : $c\n\t$ghc $threaded $ghc_flags -no-link -no-hs-main -O -o $o $c\n";
    };

    mkdir 'dist';
    mkdir 'dist/build';
    mkdir 'dist/build/src';

    postamble(fixpaths(<< "."));
$config_h : lib/Perl6/Pugs.pm util/config_h.pl
	\$(PERL) util/config_h.pl "$ghc $ghc_flags"

$version_h : .svn/entries util/version_h.pl
	\$(PERL) util/version_h.pl $version_h

.svn/entries :
	\$(NOOP)

@{[join("\n", map {$emit->($_)} ($unicode_c, $pcre_c, @cbits_c))]}

src/perl5/perl5.o : src/perl5/perl5.h src/perl5/perlxsi.c src/perl5/perl5.c src/perl5/pugsembed.c src/perl5/pugsembed.h
	$ghc $threaded $ghc_flags -no-link -no-hs-main -O -o src/perl5/perl5.o src/perl5/perl5.c

src/Pugs/Config.hs : util/PugsConfig.pm
	\$(PERL) -Iutil -MPugsConfig -e "PugsConfig->write_config_module" > src/Pugs/Config.hs

src/gen_prelude$Config{_exe} : src/gen_prelude.hs
	$ghc -O0 -o src/gen_prelude$Config{_exe} src/gen_prelude.hs

src/Pugs/CodeGen/PIR/Prelude.hs : src/gen_prelude$Config{_exe} src/perl6/Prelude/PIR.pm
	\@\$(PERL) -e "mkdir q-src/Pugs/CodeGen/PIR-"
	src/gen_prelude$Config{_exe} Pugs.CodeGen.PIR.Prelude < src/perl6/Prelude/PIR.pm > src/Pugs/CodeGen/PIR/Prelude.hs

${() = '%.hpp : %.hs @prereqs $version_h
	$ghc $threaded $ghc_flags -DHADDOCK -E \$< -o \$@
	\$(PERL) util/munge_haddock.pl \$@'; \''}

.SUFFIXES: .hs .hpp

.hs.hpp :
	$ghc $threaded $ghc_flags -DHADDOCK -E \$< -o \$@
	\$(PERL) util/munge_haddock.pl \$@

.SUFFIXES: .hsc .hs

.hsc.hs :
	$hsc2hs $hsc2hs_flags \$<

.SUFFIXES: .hs-drift .hs

.hs-drift.hs :
	\$(PERL) util/drift.pl \$< > \$@

haddock : $version_h $config_h @hppfiles docs/haddock
	haddock -t Pugs-$version -h -o docs/haddock/ @hppfiles
	\@\$(RM_F) @{[map "$_.pre", @hppfiles]} @hppfiles
	\@\$(PERL) -le "print and print q-*** API Documentation generated in @{[File::Spec->catfile('docs', 'haddock', 'index.html')]}-"

docs/haddock :
	\@\$(PERL) -e "mkdir q-docs/haddock-"

profiled :: src/Pugs/Config.hs src/Pugs/CodeGen/PIR/Prelude.hs @srcfiles $version_h $config_h config.yml src/perl6/Prelude.pm
	\$(PERL) util/build_pugs.pl _+GHC $version $ghc $ghc_version $setup $threaded -O -auto-all -prof --make $profiled_flags $ccdlflags $ghc_output _-GHC _+GEN_PRELUDE --pugs $thispugs

pugs.prof :: profiled
	find t -type f | grep -v D | grep -v R | grep -v pugsrun | ./pugs +RTS -p -RTS -e 'my sub exit {}; for =\$\$*IN -> \$\$t is copy { \$\$t .= chomp; require \$\$t }'

optimised :: optimized

optimized :: src/Pugs/Config.hs src/Pugs/CodeGen/PIR/Prelude.hs @srcfiles $version_h $config_h config.yml src/perl6/Prelude.pm
	\$(PERL) util/build_pugs.pl _+GHC $version $ghc $ghc_version $setup $threaded -O --make $ghc_flags $embed_flags $ccdlflags $ghc_output _-GHC _+GEN_PRELUDE --pugs $thispugs

unoptimised :: unoptimized

unoptimized :: src/Pugs/Config.hs src/Pugs/CodeGen/PIR/Prelude.hs @srcfiles $version_h $config_h config.yml src/perl6/Prelude.pm
	\$(PERL) util/build_pugs.pl _+GHC $version $ghc $ghc_version $setup $threaded -O0 --make $ghc_flags $embed_flags $ccdlflags $ghc_output _-GHC _+GEN_PRELUDE --pugs $thispugs

$pugs : src/Pugs/Config.hs src/Pugs/CodeGen/PIR/Prelude.hs @srcfiles $version_h $config_h config.yml src/perl6/Prelude.pm
	\$(PERL) util/build_pugs.pl _+GHC $version $ghc $ghc_version $setup $threaded __optimization__ --make $ghc_flags $embed_flags $ccdlflags $ghc_output _-GHC _+GEN_PRELUDE --pugs $thispugs


pirtest : test-pir

pirsmoke : smoke-pir

jssmoke : smoke-js


test-all : test test-js test-pir test-perl5

test-pir : 
	\$(PERL) -e "ENV->{HARNESS_PERL_SWITCHES}=q+-B PIR+; system qq+$Config{make}+, q+test+"

test-js :
	\$(PERL) -e "ENV->{HARNESS_PERL_SWITCHES}=q+-B JS+; system qq+$Config{make}+, q+test+"

test-perl5 :
	\$(PERL) -e "ENV->{HARNESS_PERL_SWITCHES}=q+-B PERL5+; system qq+$Config{make}+, q+test+"



smoke : smoke-pugs

smoke-all : smoke-pugs smoke-js smoke-pir smoke-perl5

smoke-pugs : $pugs util/run-smoke.pl all
	\$(PERL) util/run-smoke.pl . smoke.html

smoke-pir : $pugs util/run-smoke.pl all
	\$(PERL) util/run-smoke.pl . smoke-pir.html -BPIR

smoke-js : $pugs util/run-smoke.pl all
	\$(PERL) -e "ENV->{PUGS_RUNTIME}=q+JS+; exec qw+$^X util/run-smoke.pl . smoke-js.html+"

smoke-perl5 : $pugs util/run-smoke.pl all
	\$(PERL) -e "ENV->{PUGS_RUNTIME}=q+PERL5+; exec qw+$^X util/run-smoke.pl . smoke-perl5.html -BPERL5+"


ghci ::
	$ghc --interactive $ghc_flags $ghc_output

pil :: src/cbits/fpstring.o $unicode @srcfiles
	$ghc --make -H0 -L. -Lsrc -I. -Isrc -i. -isrc -static -fno-warn-name-shadowing -o pil$Config{_exe} -main-is PIL.main src/PIL.hs $unicode src/cbits/fpstring.o

pili :: src/cbits/fpstring.o $unicode @srcfiles
	$ghc --interactive -H0 -L. -Lsrc -I. -Isrc -i. -isrc -static -fno-warn-name-shadowing src/PIL.hs $unicode src/cbits/fpstring.o

tags : @srcfiles
	$hasktags -c @srcfiles
	sort tags > tags.tmp
	mv tags.tmp tags

config.yml:

src/perl6/Prelude.pm:

INST6_ARCHLIB = blib6/arch
INST6_SCRIPT = blib6/script
INST6_BIN = blib6/bin
INST6_LIB = blib6/lib
INST6_MAN1DIR = blib6/man1
INST6_MAN3DIR = blib6/man3

pure_all ::
	\$(PERLRUN) util/src_to_blib.pl
	\$(NOECHO) \$(ECHO) "*** Done!  Type '\$(MAKE) install' to install."

register ::
	$setup register

pure_site_install ::
	$setup install
	\$(NOECHO) \$(MOD_INSTALL) \\
		\$(INST6_LIB) \$(DESTDIR)$config->{sitelib} \\
		\$(INST6_ARCHLIB) \$(DESTDIR)$config->{sitearch} \\
		\$(INST6_BIN) \$(DESTDIR)$config->{sitebin} \\
		\$(INST6_SCRIPT) \$(DESTDIR)$config->{sitescript} \\
		\$(INST6_MAN1DIR) \$(DESTDIR)$config->{installsiteman1dir} \\
		\$(INST6_MAN3DIR) \$(DESTDIR)$config->{installsiteman3dir}
	\$(NOECHO) \$(ECHO) "*** Installed!  Type '\$(MAKE) register' to make Pugs a GHC package."

pure_vendor_install ::
	$setup install
	\$(NOECHO) \$(MOD_INSTALL) \\
		\$(INST6_LIB) \$(DESTDIR)$config->{privlib} \\
		\$(INST6_ARCHLIB) \$(DESTDIR)$config->{archlib} \\
		\$(INST6_BIN) \$(DESTDIR)$config->{installbin} \\
		\$(INST6_SCRIPT) \$(DESTDIR)$config->{installscript} \\
		\$(INST6_MAN1DIR) \$(DESTDIR)$config->{installman1dir} \\
		\$(INST6_MAN3DIR) \$(DESTDIR)$config->{installman3dir}
	\$(NOECHO) \$(ECHO) "*** Installed!  Type '\$(MAKE) register' to make Pugs a GHC package."
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

sub parrot_config {
    my ($base, $config) = @_;
    my $ac_path = abs_path();
    my $sp_base = $base;
    $sp_base =~ s{\\}{\/}g;
    chdir( $sp_base ) or die "Can't change dir to '$sp_base'";
    my $value = `./parrot parrot-config.imc $config`;
    chomp($value);
    chdir( $ac_path ) or die "Can't change dir to '$ac_path'";
    return $value;
}
