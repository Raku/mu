#!/usr/bin/env perl

use 5.006;
use strict;
use warnings;
use FindBin;
BEGIN { chdir $FindBin::RealBin };
use inc::Module::Install;
use lib 'inc';
use Config;
use File::Spec;
use POSIX qw/uname/;

# Hack to prevent recursive calls
exit if @ARGV and $ARGV[0] =~ /^--with/;

my ($ghc, $ghc_version, $ghc_flags, $ghc_pkg) = assert_ghc();

my $setup_exe  = File::Spec->catfile('.', "Setup$Config{_exe}");

rebuild_setup();
my $app_dir = `$setup_exe -d`;
chomp $app_dir;

die "Cannot find application directory '$app_dir'; check to see if Setup.lhs compiles?\n"
    unless $app_dir;

my $bin_dir = File::Spec->catdir($app_dir, 'bin');
my $cabal   = File::Spec->catfile($bin_dir, "cabal$Config{_exe}");

$ENV{PATH} = $bin_dir . $Config{path_sep} . $ENV{PATH};

# First let's see if Cabal is there.
print "*** Checking for Haskell dependencies...\n";
print "[Automatic Installation]\n";

my $rebuilt;
$rebuilt += bootstrap_with(Cabal            => qr/1\.4\./, '1.4.0');
#$rebuilt ||= bootstrap_with(bytestring       => qr/0\.9\.[1-9]/, '0.9.1');
$rebuilt += bootstrap_with(parsec           => qr/2\./, '2.0');
$rebuilt += bootstrap_with(network          => qr/[12]\./, '1.0');
$rebuilt += bootstrap_with(HTTP             => qr/300[01]\./, '3000.0');
$rebuilt += bootstrap_with(zlib             => qr/0\.(?:[4-9]|[1-9]\d)\./, '0.4');
$rebuilt += bootstrap_with('cabal-install'  => qr/0\.(?:[5-9]|[1-9]\d)\./, '0.5');

if ($rebuilt) {
    rebuild_setup();
}

print "[Pugs Dependencies]\n";

my @deps = qw(
    filepath mtl stm pretty time random process containers array directory 

    utf8-string
    binary
    haskeline
    FindBin
    MetaObject
    HsParrot
    pugs-compat
    pugs-DrIFT
    stringtable-atom
    HsSyck
);

#system($cabal => 'update');
system($cabal => 'install', '--user', @deps);

# TODO - At this point we'd like to fool Cabal into using our own archive.
do "Configure.PL";
print "==> Pugs configuration finished.\n";

name            ('Perl6-Pugs');
version_from    ('lib/Perl6/Pugs.pm');
abstract_from   ('lib/Perl6/Pugs.pm');
author          ('Audrey Tang <cpan@audreyt.org>');
license         ('perl');
install_script  ("pugs$Config{_exe}");
install_script  ('script/pugscc');
install_script  ('util/prove6');
build_subdirs   (map fixpaths($_), grep {
                   -f "$_/Makefile.PL" && not -l "$_/Makefile.PL"
                 } glob("docs/*") # glob("ext/*"), glob("docs/*")
                );
my $version = version();
$version .= 0 until length($version) >= length('0.123456');
$version =~ s{6\.(\d{3})(\d{3})?}{join '.', 6, int($1), int($2||0)}e;
version($version);
makemaker_args  (
    test => { TESTS => "`perl t/spec/fudgeall pugs t/*/*.t t/*/*/*.t`" }, # , "perl5/*/t/*.t" },
    MAN1PODS => {},
);
set_postamble   ();
no_index        (
    directory =>
        qw< inc debian modules perl5 ext script util docs examples src >
);
sign            (1);
WritePugs       (5);

sub set_postamble {
    unlink (my $build_config = 'current.build.yml');

    my $find_ver = sub {
        `$_[0] $_ 2>&1` =~ /(.* (?:version|build) .*)/i && return $1
            for qw{--version -v -V -? /?};
        return "unknown";
    };
    my $config = get_pugs_config(
        uname               =>  join (" ", (uname)[0,2,4] ),
        regex_engine        =>  ( $ENV{PUGS_REGEX_ENGINE} ?  $ENV{PUGS_REGEX_ENGINE} : 'default' ),
        embed_flags         => 'noparrot nohaskell',
#        embedded            =>  join (" ", ( $embed_flags =~ /HAVE_PERL5/ ?  $Config{perlpath} : 'noperl' ),
#                                    ( $embed_flags =~ /HAVE_PARROT/
#                                        ?  abs_path( File::Spec->catfile( "$ENV{PARROT_PATH}", "parrot" ) )
#                                        : 'noparrot' ),
#                                    ( $ENV{PUGS_EMBED} =~ /\bhaskell\b/ ?  $ghc : 'nohaskell' )
#                                ),
#        ghc                 =>  join (" ", [ assert_ghc() ]->[0], $ghc_version,
#                                    ( $ghc_flags =~ /HAVE_READLINE/ ?  'readline' : 'noreadline' ),
#                                    ( $ghc_flags =~ /HAVE_HSPLUGINS/ ?  'hsplugins' : 'nohsplugins' ),
#                                    ( $threaded ? "threads" : "nothreads" ),
#                                ),
        ghc => 'noreadline nohsplugins nothreads',
        cc                  =>  $find_ver->( $Config{cc} ),
    );

    {
        # maybe move this to inc/PugsBuild/MiniYAML.pm ?
        open my $fh, '>', $build_config or die "Cannot open $build_config for writing";
        print $fh "# *** NOTE ***\n";
        print $fh "# This file is generated during Makefile.PL.\n";
        print $fh "# Changes will not persist after running Makefile.PL\n";
        for ( sort keys %$config ) {
            print $fh "$_: $config->{$_}\n";
        }
        close $fh;
    }

    # Ok, now we have Cabal-Install and all the deps.  Let's be lazy and write a simple makefile.
    postamble(<<".");
pugs$Config{_exe} :: src/Pugs/Config.hs src/Pugs/Prelude.hs
\t\$(RM_F) dist/build/pugs/pugs$Config{_exe}
\t\$(RM_F) Pugs.cabal
\t\$(CP) Pugs.cabal.in Pugs.cabal
\t$cabal configure --user
\t$cabal build
\t\$(CP) dist/build/pugs/pugs$Config{_exe} .

install ::
\t$cabal install --user

INST6_ARCHLIB = blib6/arch
INST6_SCRIPT = blib6/script
INST6_BIN = blib6/bin
INST6_LIB = blib6/lib
INST6_MAN1DIR = blib6/man1
INST6_MAN3DIR = blib6/man3
INSTPUGS_LIB = blib6/pugs

src/Pugs/Config.hs : util/PugsConfig.pm current.build.yml
	\$(PERL) -Iutil -MPugsConfig -e "PugsConfig->write_config_module" > src/Pugs/Config.hs

smoke : pugs$Config{_exe} util/run-smoke.pl
	\$(PERL) util/run-smoke.pl . smoke.html

upload-smoke : smoke.yml
	\$(PERL) util/smokeserv/smokeserv-client.pl smoke.html smoke.yml

src/Pugs/Prelude.hs : src/perl6/Prelude.pm util/gen_prelude.pl
	\$(PERL) util/gen_prelude.pl -v --touch --inline -i src/perl6/Prelude.pm --output src/Pugs/Prelude.hs
.

}

sub bootstrap_with {
    my ($package, $pattern, $version) = @_;
    my @has_version;
    if ($package eq 'cabal-install') {
        @has_version = sort map { /^cabal-install version ($pattern.*)/ ? $1 : () } `$cabal --version`;
    }
    else {
        @has_version = sort map { /^version: ($pattern.*)/ ? $1 : () } `$ghc_pkg describe $package`;
    }

    printf "- %-20s ", $package;
    if (@has_version) {
        print "...loaded. ($has_version[0] >= $version)\n";
        return;
    }

    print "...missing. (would need $version)\n";
    print "==> Installing $package. This may take a minute or two...\n";
    sleep 1;

    chdir "third-party/$package";

    my ($setup_hs) = glob('Setup.*hs');
    die "Impossible: No Setup?" unless $setup_hs;

    system($ghc, qw(--make -O0), -o => $setup_exe, $setup_hs)
        || system($setup_exe, qw(configure --user))
        || system($setup_exe, qw(build))
        || system($setup_exe, qw(install --user))
        || system($setup_exe, qw(clean))
            and die << ".";
Cannot install $package: $?
Please enter the third-party/Cabal directory and install it manually
following the instructions of README there.
.

    chdir $FindBin::RealBin;
    return 1;
}

sub for_perl5 {
    my $cmd = shift;
    $cmd =~ s{\n}{}g;
    my @cmds;
    foreach my $dir (grep { -d } glob('perl5/*')) {
        -e "$dir/Makefile.PL" or next;

        # Skip XS modules for now
        next if glob("$dir/*.xs") or glob("$dir/*.i") or $dir =~ /-\d+/;

        my $this = $cmd;
        $this =~ s{__DIR__}{$dir}g;
        push @cmds, $this;
    }
    return join("\n", @cmds);
}

sub rebuild_setup {
    unlink $_ for qw(Setup.hi Setup.o);
    unlink $setup_exe;
    system($ghc, qw(--make -O0), -o => $setup_exe, 'Setup.lhs');
}
