#!/usr/bin/perl -w

use strict;
use warnings;
use File::Copy qw(copy);
use File::Path qw(mkpath rmtree);
use File::Find qw(find);

our %BuildPrefs;
use Config;
use FindBin;
BEGIN { chdir $FindBin::RealBin; chdir '..'; };
use lib 'inc';
use PugsBuild::Config;

help() if ($ARGV[0] || '--help') =~ /^--?h(?:elp)?/i;
build(classify_options(@ARGV));
exit 0;

sub help {
    print <<".";
$0 - build a pugs executable

This script calls GHC to build a pugs exectuable, optionally inlining
precompiled modules in a second pass.

Primary configuration settings are read from the file `config.yml` in
the build root. You may override these settings using the PUGS_BUILD_OPTS
environment variable.

Current settings:
.
    print PugsBuild::Config->pretty_print;

    exit 0;
}

my $run_setup;

sub build {
    my($opts) = @_;
    my $thispugs = { @{ $opts->{GEN_PRELUDE} } }->{'--pugs'} or # laugh at me now.
        die "$0: no pugs passed in _+GEN_PRELUDE segment";
    
    print "Build configuration:\n" . PugsBuild::Config->pretty_print;

    my ($version, $ghc, $ghc_version, $setup, @args) = @{$opts->{GHC}};
    write_buildinfo($version, $ghc_version, @args);
    $run_setup = sub { system($setup, @_) };
    $run_setup->('configure', grep !/^--.*=$/, @{$opts->{SETUP}});

    # if Prelude.pm wasn't changed, don't bother to recompile Run.hs.
    if (PugsBuild::Config->lookup('precompile_prelude')) {
        my $pm = "src/perl6/Prelude.pm";
        my $ppc_hs = "src/Pugs/PreludePC.hs";
        my $ppc_null = "src/Pugs/PreludePC.hs-null";
        if (-e $ppc_hs and -s $ppc_hs > -s $ppc_null and -M $ppc_hs < -M $pm) {
            build_lib($version, $ghc, @args);
            build_exe($version, $ghc, $ghc_version, @args);
            return;
        }
    }

    run($^X, qw<util/gen_prelude.pl -v --touch --null --output src/Pugs/PreludePC.hs>);
    build_lib($version, $ghc, @args);
    build_exe($version, $ghc, $ghc_version, @args);

    if (PugsBuild::Config->lookup('precompile_prelude')) {
        run($^X, qw<util/gen_prelude.pl -v -i src/perl6/Prelude.pm>,
                (map { ('-i' => $_) } @{ PugsBuild::Config->lookup('precompile_modules') }),
                '-p', $thispugs, qw<--touch --output src/Pugs/PreludePC.hs>);
        build_lib($version, $ghc, @args);
        build_exe($version, $ghc, $ghc_version, @args);
    }
}

sub build_lib {
    my $version = shift;
    my $ghc     = shift;

    my $ar = $Config{full_ar};
    my $a_file = File::Spec->rel2abs("dist/build/libHSPugs-$version.a");

    unlink $a_file;
    $run_setup->('build');
    die "Build failed: $?" unless -e $a_file;

    if (!$ar) {
        $ar = $ghc;
        $ar =~ s{(.*)ghc}{$1ar};
    }

    my $fixup = sub {
        my $module = shift; # eg. "Data.Yaml.Syck"
        my $pathname = $module;
        $pathname =~ s!\.!/!g;
        $pathname .= '_stub.o';
        my $basename = $pathname;
        $basename =~ s!.*/!!;

        # XXX - work around Cabal bug --
        # we have to locate "Syck_stub.o" and copy it into
        # dist/build/src/Data/Yaml/.
        my @candidates;
        my $target = File::Spec->canonpath("dist/build/src/$pathname");
        my $wanted = sub {
            return unless $_ eq $basename;
            push @candidates, $File::Find::name;
        };
        find $wanted, "dist";

        if (@candidates > 1) {
            warn "*** Found more than one '$basename' -- using first one. \n";
        }
        elsif (@candidates == 0) {
            die "*** Wasn't able to find '$basename', aborting...\n";
        }

	unless( File::Spec->canonpath($candidates[0]) eq $target ) {
	    copy($candidates[0] => $target);
	}

        system($ar, r => $a_file, "dist/build/src/$pathname");
    };

    $fixup->('Data.Yaml.Syck');
    $fixup->('Pugs.Embed.Perl5') if grep /^-DPUGS_HAVE_PERL5$/, @_;
    $fixup->('Pugs.Embed.Parrot') if grep /^-DPUGS_HAVE_PARROT$/, @_;

    # system($ar, r => $a_file, $_) for grep /\.(?:o(?:bj)?)$/, @_;

    foreach my $a_ext (grep /\.a$/, @_) {
        # Do some very sneaky things -- linking other .a with us!
        my $basename = $a_ext;
        $basename =~ s!.*/!!;
        my $dir = "dist/tmp-$basename";
        mkdir $dir;
        chdir $dir;
        system($ar, x => $a_ext);
        system($ar, r => $a_file, glob("*"));
        unlink(glob("*"));
        chdir '..';
        chdir '..';
        rmdir $dir;
    }

    # Run ranlib.
    system($ar, s => $a_file);
}

sub build_exe {
    my $version     = shift;
    my $ghc         = shift;
    my $ghc_version = shift;
    #my @o = qw( src/pcre/pcre.o src/syck/bytecode.o src/syck/emitter.o src/syck/gram.o src/syck/handler.o src/syck/implicit.o src/syck/node.o src/syck/syck.o src/syck/syck_st.o src/syck/token.o src/syck/yaml2byte.o src/cbits/fpstring.o );
    #push @o, 'src/UnicodeC.o' if grep /WITH_UNICODEC/, @_;
    #system $ghc, '--make', @_, @o, '-o' => 'pugs', 'src/Main.hs';
    my @pkgs = qw(-package stm -package network -package mtl -package template-haskell -package base);
    if ($^O =~ /(?:MSWin32|mingw|msys|cygwin)/) {
        push @pkgs, -package => 'Win32' unless $ghc_version =~ /^6.4(?:.0)?$/;
    }
    else {
        push @pkgs, -package => 'unix';
    }
    push @pkgs, -package => 'readline' if grep /^-DPUGS_HAVE_READLINE$/, @_;
    push @pkgs, -package => 'plugins', -package => 'haskell-src' if grep /^-DPUGS_HAVE_HSPLUGINS$/, @_;
    my @libs = "-lHSPugs-$version";
    push @libs, grep /\.a$/, @_;
    push @libs, grep /^-[lL]/, @_;
    push @libs, grep /\.o$/, @_;

    @_ = (@pkgs, qw(-idist/build -Ldist/build -idist/build/src -Ldist/build/src -o pugs src/Main.hs), @libs);
    print "*** Building: ", join(' ', $ghc, @_), $/;
    system $ghc, @_;

    die "Build failed: $?" unless -e "pugs$Config{_exe}";
}

sub write_buildinfo { 
    my $version = shift;
    my $ghc_version = shift;

    open IN, "< Pugs.cabal.in" or die $!;
    open OUT, "> Pugs.cabal" or die $!;

    my $depends = '';
    if ($^O =~ /(?:MSWin32|mingw|msys|cygwin)/) {
        $depends = ', Win32 -any' unless $ghc_version =~ /^6.4(?:.0)?$/;
    }
    else {
        $depends = ', unix -any';
    }

    if (grep /^-DPUGS_HAVE_READLINE$/, @_) {
        $depends .= ', readline -any';
    }

    my $unicode_c = '';
    if ($ghc_version =~ /^6\.4(?:\.0)?$/) {
        $unicode_c = 'src/UnicodeC.c';
    }

    my $perl5_c = '';
    if (grep /^-DPUGS_HAVE_PERL5$/, @_) {
        $perl5_c = 'src/perl5/p5embed.c';
    }

    my @include_dirs = map substr($_, 2), grep /^-I/, @_;
    my @lib_dirs = map substr($_, 2), grep /^-L/, @_;
    my @libs = map substr($_, 2), grep /^-l/, @_;
    #push @libs, grep /\.(?:a|o(?:bj)?)$/, @_;

    while (<IN>) {
        s/__OPTIONS__/@_/;
        s/__VERSION__/$version/;
        s/__DEPENDS__/$depends/;
        s/__UNICODE_C__/$unicode_c/;
        s/__PERL5_C__/$perl5_c/;
        s/__INCLUDE_DIRS__/@include_dirs/;
        s/__LIBS__/@libs/;
        s/__LIB_DIRS__/@lib_dirs/;
        print OUT $_;
    }

    close IN;
    close OUT;
}

sub classify_options {
    my($kind, %opts);
    for (@_) {
        # we can't use +SEGMENT and -SEGMENT since that interferes with GHC.
        $kind = $1,  next if /^_\+(.*)/;        # _+SEGMENT start
        undef $kind, next if $_ eq "_-$kind";   # _-SEGMENT end
        
        s/^__(.*)__$/PugsBuild::Config->lookup($1)/e;
        
        die "don't know where this option belongs: $_" unless $kind;
        push @{ $opts{$kind} }, $_;
    }
    \%opts;
}

sub run {
    print ((join " ", @_) . "\n");
    system @_ and die (sprintf "system: [%s]: $!", join " ", @_);
}

sub copy_all {
    my ($src, $dest) = @_;
    mkpath($dest);
    local *DIR;
    opendir(DIR, $src) or die $!;
    my @nodes = readdir(DIR);
    foreach my $node (sort @nodes) {
        next if $node =~ /^(\.|\.\.|\.svn|src)$/;
        my $src_path = "$src/$node";
        my $dest_path = "$dest/$node";
        if (-f $src_path) {
            copy($src_path, $dest_path);
        }
        if (-d $src_path) {
            copy_all($src_path, $dest_path);
        }
    }
}
