#!/usr/bin/perl

=pod

Build System Overview

 - Makefile.PL calls GHC to compile Setup.hs to ./Setup.exe.
 - Makefile.PL writes a Makefile that will call util/build_pugs.pl
   upon "make".
 - build_pugs.pl writes a Pugs.cabal file based on the flags given in
   the Makefile.
 - build_pugs.pl calls ./Setup.exe.
 - Setup.exe builds dist/build/libHSPugs-6.XXX.YYY.a and returns to
   build_pugs.pl.
 - build_pugs.pl calls GHC to build ./pugs.exe
   (by simply linking the freshly built libHSPugs package with src/Main.hs).

=cut


use 5.006;
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
use Carp;

# Win32/CygWin GHC appears to be more memory hungry than its
# *nix counterpart, causing it to bail part-way through builds.
# This value will be used only if 'ghc_heap_size' in config.yml
# is not defined.

use constant WIN32_GHC_HEAPSIZE => "348m";

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
author          ('Audrey Tang <autrijus@autrijus.org>');
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
    test => { TESTS => join ' ', "t/*/*.t", "t/*/*/*.t" }, # , "perl5/*/t/*.t" },
    MAN1PODS => {},
);

if (my $prefix = PugsBuild::Config->lookup('install_dir')) {
    makemaker_args(PREFIX => $prefix);
}

clean_files     (map fixpaths($_),
    $version_h, $config_h,
    (map {"$_/*.hpp"} @srcdirs),
    "pugs$Config{_exe}", "pil$Config{_exe}",
    "src/gen_prelude$Config{_exe}",
    "Setup$Config{_exe}", qw(
    src/Pugs/pugs_config.h src/Pugs/Config.hs blib6 dist
    src/Pugs/Prelude.hs src/Pugs/CodeGen/PIR/Prelude.hs test.log
    src/Pugs/Embed/Parrot.hs src/Pugs/Embed/Parrot_hsc.*
    src/Pugs/Run/Perl5_stub.* src/Data/Yaml/Syck_stub.* src/Data/Yaml/Syck.hs
    temp-ex* unlink-test* Prelude Pugs.cabal Setup.hi Setup.o
    .setup-config .installed-pkg-config
    temp-test.*.*-out tempfile.*.* create_this_file create_this_file2
    )
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

    my ($ghc, $ghc_version, $ghc_flags, $ghc_pkg) = assert_ghc();
    my $hsc2hs = $ENV{HSC2HS};
    my $setup = File::Spec->catfile(".", "Setup$Config{_exe}");

    if (!$hsc2hs) {
        $hsc2hs = $ghc;
        $hsc2hs =~ s{(.*)ghc}{$1hsc2hs};
    }

    unlink("Setup$Config{_exe}");
    unlink("Setup.o");
    unlink("Setup.hi");
    my $rv = system($ghc, '--make', '-o' => $setup, 'Setup.lhs');
    unless (-s "Setup$Config{_exe}") {
        die << ".";
*** Building Setup$Config{_exe} failed (exit code $rv)
    Please check your GHC and Cabal installation.
.
    }

    my $heap;

    # Win32 GHC is a memory hog.  We need to provide it with a memory sty.
    # We still use the option from config.yml if available.

    if ($Config{osname} eq 'cygwin' or $Config{osname} eq 'MSWin32') {
        $heap = WIN32_GHC_HEAPSIZE;
    }

    $heap = PugsBuild::Config->lookup('ghc_heap_size') || $heap;
    $ghc_flags .= " +RTS -M$heap -RTS" if $heap;
    # $ghc_flags .= ' -dcore-lint';
    # $ghc_flags .= " -keep-tmp-files";
    if (has_ghc_package('plugins')
        and  try_compile("import System.Eval\n"
                        ."main :: IO ()\n"
                        .'main = (eval_ "return ()" [] [] [] [] :: IO (Either [String] (Maybe ()))) >> return ()', $ghc, '-package', 'plugins')) {
        $ghc_flags .= ' -package plugins -DPUGS_HAVE_HSPLUGINS ';
    } else {
        push @warn, << '.';
*** Inline Haskell support disabled.  If you want dynamic loading
    of haskell modules, please install the hs-plugins library:
        http://www.cse.unsw.edu.au/~dons/hs-plugins/
    Remember to "make register" after "make install" for hs-plugins!

.
    }

    if ($ghc_version =~ /^6.4(?:.0)?$/) {
      push @warn, << '.';
*** Readline support disabled.  If you want readline support,
    please upgrade to GHC 6.4.1 or later.

.
    }
    elsif (has_ghc_package('readline') 
        and  try_compile("import System.Console.Readline\n"
                        ."main :: IO ()\n"
                        ."main = do\n"
                        ."  setCatchSignals False\n"
                        ."  setCatchSigwinch False\n"
                        .'  _ <- readline "" '."\n"
                        .'  return ()', $ghc)) {
      $ghc_flags .= ' -DPUGS_HAVE_READLINE '; # -package readline';
    }
    else {
      push @warn, << '.';
*** Readline support disabled.  If you want readline support,
    please install the GNU readline library.

.
    }

    my $ghc_output = ''; # "-o pugs$Config{_exe} src/Main.hs";
    my $hasktags = $ENV{HASKTAGS} || 'hasktags';

    # map a bunch of .c files to a bunch of expected .o files
    my $o = sub { map { substr($_, 0, -1) . 'o' } @_ };

    my $pcre_c    = "src/pcre/pcre.c";
    my $unicode_c = "src/UnicodeC.c";
    my @cbits_c   = glob("src/cbits/*.c");
    my @syck_c    = glob("src/syck/*.c");
    my ($pcre)    = $o->($pcre_c);
    my ($unicode) = $o->($unicode_c);
    my @cbits     = $o->(@cbits_c);
    my @syck      = $o->(@syck_c);

    my @prereqs = ($pcre, @syck, @cbits);
    #$ghc_output .= " $pcre @cbits $unicode";

    my $embed_flags = "";
    my $hsc2hs_flags = "";
    my $ccdlflags = "";

    if ($ENV{PUGS_EMBED} and $ENV{PUGS_EMBED} =~ /\bperl5\b/i  and !($Config{cc} eq 'cl') )  {
        push @prereqs, "src/perl5/p5embed.o"; # XXX
        # $ghc_output .= " src/perl5/p5embed.o ";
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
        push @prereqs, $unicode;
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
    Please download the GHC 6.4.1 installer from http://haskell.org/ghc/.
    (Alternatively, use "sudo gcc_select 3.3" to pick an earlier version
     of GCC, or set the CC environment variable to "/usr/bin/gcc-3.3".)
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
        my $parrot_config = which('parrot-config.imc') || which('parrot-config.pir') || which('parrot-config') || '';
        if (!$base && -e $parrot_config) {
            $base = (File::Spec->splitpath($parrot_config))[1];
        }
        if (!$base) {
            $parrot_config = which('parrot-config') || '';
            $base = (File::Spec->splitpath($parrot_config))[1] if -e $parrot_config;
        }
        if (!$base and -d File::Spec->catdir(File::Spec->updir, 'parrot')) {
            $base = abs_path(File::Spec->catdir(File::Spec->updir, 'parrot'));
        }
        if (!$base and -d File::Spec->catdir(File::Spec->updir, 'parrot-trunk')) {
            $base = abs_path(File::Spec->catdir(File::Spec->updir, 'parrot-trunk'));
        }

        my $temp = File::Spec->catfile($base, 'parrot-config.imc');
        if (!$parrot_config && -e $temp) {
          $parrot_config = $temp;
        }
        
        $temp = File::Spec->catfile($base, 'parrot-config.pir');
        if (!$parrot_config && -e $temp) {
          $parrot_config = $temp;
        }
        
        $temp = File::Spec->catfile($base, 'parrot-config');
        if (!$parrot_config && -e $temp) {
          $parrot_config = $temp;
        }

        $base =~ s/bin\/*$//;
        
        (-d $base and -e $parrot_config)
            or die "*** Please set \$ENV{PARROT_PATH} to the base path with a built parrot tree.\n";
        my $ldflags = parrot_config($base, $parrot_config, 'ldflags');
        my $libs = parrot_config($base, $parrot_config, 'libs');
        my $icuflags = parrot_config($base, $parrot_config, 'icu_shared');
        my $include_path = parrot_config($base, $parrot_config, 'prefix') . parrot_config($base, $parrot_config, 'inc');
        my $rpath_blib = parrot_config($base, $parrot_config, 'rpath_blib');
        my $build_dir = parrot_config($base, $parrot_config, 'top_builddir', 'build_dir');
        my $is_shared = parrot_config($base, $parrot_config, 'parrot_is_shared');

        # Convert flags to -optc-*,etc, to pass through ghc. c/ompiler l/inker
        $ldflags =~ s/(^|\s)-/$1-optl-/g;
        $libs =~ s/(^|\s)-/$1-optl-/g;
        $icuflags =~ s/(^|\s)-/$1-optl-/g;
        $include_path =~ s/(^|\s)-/$1-optc-/g;
        $embed_flags .= " -I$include_path" if $include_path =~ /\S/;
        $rpath_blib =~ s/(^|\s)-/$1-optl-/g;

        $embed_flags .= " -I$base/include -L$base/lib -L$base/blib/lib -DPUGS_HAVE_PARROT -L/usr/local/lib $ldflags ";
        $ghc_output .= " -lparrot $libs $icuflags ";
        my $config = "$build_dir/src/install_config$Config{_o}";
        $config = "$build_dir/src/parrot_config$Config{_o}" unless -e $config;
        $config = "$build_dir/src/null_config$Config{_o}" unless -e $config;

        die <<"EOD" unless -e $config or $is_shared; 
*** Could not find src/null_config.o in $build_dir.
    Solutions include:
        * Remove parrot from PUGS_EMBED
        * Place a built Parrot source tree under $build_dir.
EOD

        $ghc_output .= " $config ";
        $ccdlflags .= " $rpath_blib ";

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

    if ( try_compile("main :: IO ()\nmain = return ()", $ghc, "-lz") ) {
        $ghc_flags .= ' -DUSE_ZLIB ';
        $ghc_output .= " -lz ";
    }

    $hsc2hs_flags .= " -Isrc/syck "; # for Data.Yaml

    my $config = get_pugs_config();
    my $is_win32 = ($^O =~ /MSWin|mingw|cygwin/i);
    my $threaded = (!$is_win32 and try_compile("main :: IO ()\nmain = return ()", $ghc, "-threaded"))
        ? '-threaded' : '';

    if ($threaded and $ENV{PUGS_EMBED} and $ENV{PUGS_EMBED} =~ /\bperl5\b/ and !$Config{usethreads}) {
        push @warn, << '.';
*** Thread support disabled because you are building with embedded perl5 and
    your perl5 is not threaded.

.

        $threaded = '';
    }

    # As of now, Test::TAP::HTMLMatrix is the key dependency for smokes,
    # so we need only check for that.
    eval { require Test::TAP::HTMLMatrix } or push @warn, << '.';
*** You do not appear to have a Smoke Kit installed. You can still build
    Pugs and even run `make test', but you won't be able to run the more
    modern `make smoke' target to produce nice graphs and send them to the
    public smoke server. Installing Bundle::Pugs::SmokeKit from CPAN will
    bring in the necessary dependencies.

.
    # XXX - hsplugins doesn't build with profiles
    my $profiled_flags = $ghc_flags;
    $profiled_flags =~ s{-DPUGS_HAVE_HSPLUGINS}{};
    $profiled_flags =~ s{-package plugins}{};

    my $emit = sub {
        my $o = shift;
        my $c = substr($o, 0, -1) . 'c';
        return "$o : $c\n\t$ghc $threaded $ghc_flags -no-link -no-hs-main -O -o $o $c\n";
    };

    mkdir 'dist';
    mkdir 'dist/build';
    mkdir 'dist/build/src';

    # Using the Win32 Haskell compiler results in a setup program that
    # doesn't grok cygwin paths.  This kludge takes any /cygdrive/
    # style paths and rewrites them.  This really should be factored
    # out to a common module.  Better still, we should make a Cygwin-native
    # GHC.

    if ($Config{osname} eq 'cygwin') {

        # NB.  We're exploiting for's aliasing of variables.
        foreach my $path ($ghc_pkg, $hsc2hs, $ghc) {
            $path =~ s{^/cygdrive/(\w)/}{$1:/};
        }
    }

    my $setup_flags = "--prefix=\$(DESTDIR) --with-hc-pkg=$ghc_pkg --with-hsc2hs=$hsc2hs --ghc --with-compiler=$ghc";

    postamble(fixpaths(<< "."));
$config_h : lib/Perl6/Pugs.pm util/config_h.pl
	\$(PERL) util/config_h.pl "$ghc $ghc_flags"

$version_h : .svn/entries util/version_h.pl
	\$(PERL) util/version_h.pl $version_h

.svn/entries :
	\$(NOOP)

@{[join("\n", map {$emit->($_)} grep { /\.o$/ } @prereqs)]}

src/Pugs/Config.hs : util/PugsConfig.pm
	\$(PERL) -Iutil -MPugsConfig -e "PugsConfig->write_config_module" > src/Pugs/Config.hs

src/gen_prelude$Config{_exe} : src/gen_prelude.hs
	$ghc -O0 -o src/gen_prelude$Config{_exe} src/gen_prelude.hs

src/Pugs/CodeGen/PIR/Prelude.hs : src/gen_prelude$Config{_exe} src/perl6/Prelude/PIR.pm
	\@\$(PERL) -e "mkdir q-src/Pugs/CodeGen/PIR-"
	src/gen_prelude$Config{_exe} Pugs.CodeGen.PIR.Prelude < src/perl6/Prelude/PIR.pm > src/Pugs/CodeGen/PIR/Prelude.hs

src/Pugs/Prelude.hs : src/perl6/Prelude.pm util/gen_prelude.pl
	\$(PERL) util/gen_prelude.pl -v --touch --inline -i src/perl6/Prelude.pm --output src/Pugs/Prelude.hs

${() = '%.hpp : %.hs @prereqs $version_h
	$ghc $threaded $ghc_flags -DHADDOCK -E \$< -o \$@
	\$(PERL) util/munge_haddock.pl \$@'; \''}

.SUFFIXES: .hs .hpp

.hs.hpp :
	$ghc $threaded $ghc_flags -DHADDOCK -E \$< -o \$@
	\$(PERL) util/munge_haddock.pl \$@

.SUFFIXES: .hsc .hs

.hsc.hs :
	\$(PERL) -MFile::Spec -e "sub p () { File::Spec->splitpath(ARGV->[0]) }; chdir((p)[1]); system(q($hsc2hs), qw($hsc2hs_flags), (p)[2]);" \$<

.SUFFIXES: .grammar .hs

.grammar.hs :
	\$(PERL) util/file_to_hs.pl \$< \$@

.SUFFIXES: .pil .hs

.pil.hs :
	\$(PERL) util/file_to_hs.pl \$< \$@

src/Emit/PIR.hs:
src/Pugs/AST/Internals.hs:
src/Pugs/PIL1.hs:
src/Pugs/PIL2.hs:
util/drift.pl:

src/Emit/PIR/Instances.hs: src/Emit/PIR.hs util/drift.pl
	\$(PERL) util/drift.pl src/Emit/PIR.hs

src/Pugs/AST/Internals/Instances.hs: src/Pugs/AST/Internals.hs util/drift.pl
	\$(PERL) util/drift.pl src/Pugs/AST/Internals.hs

src/Pugs/PIL1/Instances.hs: src/Pugs/PIL1.hs util/drift.pl
	\$(PERL) util/drift.pl src/Pugs/PIL1.hs

src/Pugs/PIL2/Instances.hs: src/Pugs/PIL2.hs util/drift.pl
	\$(PERL) util/drift.pl src/Pugs/PIL2.hs	

haddock : $version_h $config_h @hppfiles dist/doc/html
	haddock -t Pugs-$version -h -o dist/doc/html/ @hppfiles
	\@\$(RM_F) @{[map "$_.pre", @hppfiles]} @hppfiles
	\@\$(PERL) -le "print and print q-*** API Documentation generated in @{[File::Spec->catfile('dist', 'doc', 'html', 'index.html')]}-"

# make haddock one file at a time to get partial, unlinked output
haddock-broken : $version_h $config_h @hppfiles dist/doc/html
@{[join("\n", map {"\thaddock -t Pugs-$version -h -o dist/doc/html/ $_"} @hppfiles)]}
	\@\$(RM_F) @{[map "$_.pre", @hppfiles]} @hppfiles
	\@\$(PERL) -le "print and print q-*** Unlinked API Documentation generated in @{[File::Spec->catfile('dist', 'doc', 'html', 'index.html')]}-"

dist/doc/html :
	\@\$(PERL) -MFile::Path -e "mkpath q-dist/doc/html-"

pugs_requirements : src/Emit/PIR/Instances.hs src/Pugs/AST/Internals/Instances.hs src/Pugs/PIL1/Instances.hs src/Pugs/PIL2/Instances.hs src/Pugs/Config.hs src/Pugs/CodeGen/PIR/Prelude.hs @srcfiles $version_h $config_h config.yml src/perl6/Prelude.pm src/Pugs/Prelude.hs

profiled :: pugs_requirements
	\$(PERL) util/build_pugs.pl _+SETUP $setup_flags _-SETUP _+GHC $version $ghc $ghc_version $setup --make -O -auto-all -prof $threaded $profiled_flags $ccdlflags $embed_flags $ghc_output _-GHC _+GEN_PRELUDE --pugs $thispugs

pugs.prof :: profiled
	find t -type f | grep -v D | grep -v R | grep -v pugsrun | ./pugs +RTS -p -RTS -e 'my sub exit {}; for =\$\$*IN -> \$\$t is copy { \$\$t .= chomp; require \$\$t }'

optimised :: optimized

optimized :: pugs_requirements
	\$(PERL) util/build_pugs.pl _+SETUP $setup_flags _-SETUP _+GHC $version $ghc $ghc_version $setup --make -O $threaded $ghc_flags $ccdlflags $embed_flags $ghc_output _-GHC _+GEN_PRELUDE --pugs $thispugs

unoptimised :: unoptimized

unoptimized :: pugs_requirements
	\$(PERL) util/build_pugs.pl _+SETUP $setup_flags _-SETUP _+GHC $version $ghc $ghc_version $setup --make -O0 $threaded $ghc_flags $ccdlflags $embed_flags $ghc_output _-GHC _+GEN_PRELUDE --pugs $thispugs

$pugs : pugs_requirements
	\$(PERL) util/build_pugs.pl _+SETUP $setup_flags _-SETUP _+GHC $version $ghc $ghc_version $setup --make __optimization__ $threaded $ghc_flags $ccdlflags $embed_flags $ghc_output _-GHC _+GEN_PRELUDE --pugs $thispugs


pirtest : test-pir

pirsmoke : smoke-pir

jssmoke : smoke-js

test :: blib6/lib/Test.pm.yml

test-all : test test-js test-pir test-perl5

test-pir : 
	\$(PERL) -e "ENV->{HARNESS_PERL_SWITCHES}=q+-B PIR+; system qq+$Config{make}+, q+test+"

test-js :
	\$(PERL) -e "ENV->{HARNESS_PERL_SWITCHES}=q+-B JS+; system qq+$Config{make}+, q+test+"

test-perl5 :
	\$(PERL) -e "ENV->{HARNESS_PERL_SWITCHES}=q+-B PERL5+; system qq+$Config{make}+, q+test+"

blib6/lib/Test.pm.yml :
	\$(ECHO) "Precompiling Test.pm..."
	$thispugs -CParse-YAML ext/Test/lib/Test.pm > blib6/lib/Test.pm.yml 

smoke : smoke-pugs

smoke-all : smoke-pugs smoke-js smoke-pir smoke-perl5

smoke-pugs : $pugs util/run-smoke.pl all blib6/lib/Test.pm.yml
	\$(PERL) util/run-smoke.pl . smoke.html

smoke-pir : $pugs util/run-smoke.pl all
	\$(PERL) util/run-smoke.pl . smoke-pir.html -BPIR

smoke-jsperl5 : $pugs util/run-smoke.pl all
	\$(PERL) -e "ENV->{PUGS_RUNTIME}=q+JSPERL5+; exec qw+$^X util/run-smoke.pl . smoke-jsperl5.html+"

smoke-js : $pugs util/run-smoke.pl all
	\$(PERL) -e "ENV->{PUGS_RUNTIME}=q+JS+; exec qw+$^X util/run-smoke.pl . smoke-js.html+"

smoke-perl5 : $pugs util/run-smoke.pl all
	\$(PERL) -e "ENV->{PUGS_RUNTIME}=q+PERL5+; exec qw+$^X util/run-smoke.pl . smoke-perl5.html -BPERL5+"


ghci : pugs @prereqs
	$ghc $ghc_flags --interactive -fglasgow-exts -L. -idist/build -Ldist/build -idist/build/src -Ldist/build/src -isrc src/Main.hs @prereqs

pil$Config{_exe} : $config_h $unicode $pcre @cbits @srcfiles src/PIL/Native/Bootstrap.hs src/PIL/Native/Syntax.hs
	$ghc $ghc_flags --make -fglasgow-exts -H0 -isrc -isrc/cbits -Isrc -Isrc/cbits -L. -fno-warn-name-shadowing -o pil$Config{_exe} -main-is PIL.main src/PIL.hs $unicode $pcre @cbits

pili : pil$Config{_exe}
	$ghc --interactive -fglasgow-exts -isrc -isrc/cbits -Isrc -Isrc/cbits -L. -static -fno-warn-name-shadowing src/PIL.hs $unicode $pcre @cbits

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
INSTPUGS_LIB = blib6/pugs

build_perl5 ::
@{[for_perl5("
	cd __DIR__ && perl Makefile.PL && \$(MAKE)
")]}

clean ::
@{[for_perl5("
	-cd __DIR__ && \$(TEST_F) \$(FIRST_MAKEFILE) && \$(MAKE) clean
")]}

realclean ::
@{[for_perl5("
	-cd __DIR__ && \$(TEST_F) \$(FIRST_MAKEFILE) && \$(MAKE) realclean
")]}

pure_all :: build_perl5
	\$(PERLRUN) util/src_to_blib.pl

register ::
	\$(PERLRUN) util/ghc_setup.pl register --prefix=\$(DESTDIR)

pure_site_install ::
	\$(NOECHO) \$(MOD_INSTALL) \\
		\$(INST6_LIB) \$(DESTDIR)$config->{sitelib} \\
		\$(INST6_ARCHLIB) \$(DESTDIR)$config->{sitearch} \\
		\$(INST6_BIN) \$(DESTDIR)$config->{sitebin} \\
		\$(INST6_SCRIPT) \$(DESTDIR)$config->{sitescript} \\
		\$(INST6_MAN1DIR) \$(DESTDIR)$config->{installsiteman1dir} \\
		\$(INST6_MAN3DIR) \$(DESTDIR)$config->{installsiteman3dir} \\
		\$(INSTPUGS_LIB) \$(DESTDIR)$config->{sitelib}/auto/pugs
	\$(PERLRUN) util/ghc_setup.pl copy --copy-prefix=\$(DESTDIR)

pure_vendor_install ::
	\$(NOECHO) \$(MOD_INSTALL) \\
		\$(INST6_LIB) \$(DESTDIR)$config->{privlib} \\
		\$(INST6_ARCHLIB) \$(DESTDIR)$config->{archlib} \\
		\$(INST6_BIN) \$(DESTDIR)$config->{installbin} \\
		\$(INST6_SCRIPT) \$(DESTDIR)$config->{installscript} \\
		\$(INST6_MAN1DIR) \$(DESTDIR)$config->{installman1dir} \\
		\$(INST6_MAN3DIR) \$(DESTDIR)$config->{installman3dir} \\
		\$(INSTPUGS_LIB) \$(DESTDIR)$config->{privlib}/auto/pugs
	\$(PERLRUN) util/ghc_setup.pl copy --copy-prefix=\$(DESTDIR)
.
}

sub for_perl5 {
    my $cmd = shift;
    $cmd =~ s{\n}{}g;
    my @cmds;
    foreach my $dir (grep -d && -e "$_/Makefile.PL", glob('perl5/*')) {
        my $this = $cmd;
        $this =~ s{__DIR__}{$dir}g;
        push @cmds, $this;
    }
    return join("\n", @cmds);
}

sub try_compile_and_run { try_compile(@_, 'run') }

sub try_compile {
    my $code = shift;
    my $temp = "pugs-tmp-$$";
    my $ghc  = shift or croak "try_compile called without path to ghc";

    eval {
        open TMP, "> $temp.hs";
        print TMP $code;
        close TMP;
        system(
            $ghc, @_,
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

sub parrot_config {
    my $base = shift;
    my $parrot_config = shift;
    my $ac_path = abs_path();
    my $sp_base = $base;
    $sp_base =~ s{\\}{\/}g;
    chdir( $sp_base ) or die "Can't change dir to '$sp_base'";
    my $parrot = "parrot$Config{_exe}";
    $parrot = "bin/$parrot" if not -e $parrot;
    my $value;
    while (@_) {
        my $config = shift;
        $value = `./$parrot $parrot_config $config`;
        $value =~ /no such key:/ or last;
    }
    die $value if $value =~ /no such key:/;
    chomp($value);
    chdir( $ac_path ) or die "Can't change dir to '$ac_path'";
    return $value;
}

sub which {
    my $file = shift;
    for ( File::Spec->path() ) {
        my $full_name = File::Spec->catfile($_, $file);
        return $full_name if -e $full_name;
    }
    return undef;    
}
