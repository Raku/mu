#!/usr/bin/perl -w

use strict;
use warnings;
use File::Copy qw(copy);
use File::Path qw(mkpath rmtree);

our %BuildPrefs;
use Config;
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

sub build {
    my($opts) = @_;
    my $thispugs = { @{ $opts->{GEN_PRELUDE} } }->{'--pugs'} or # laugh at me now.
        die "$0: no pugs passed in _+GEN_PRELUDE segment";
    
    print "Build configuration:\n" . PugsBuild::Config->pretty_print;

    my ($ghc, $setup, @args) = @{$opts->{GHC}};
    write_buildinfo(@args);

    # if Prelude.pm wasn't changed, don't bother to recompile Run.hs.
    if (PugsBuild::Config->lookup('precompile_prelude')) {
        my $pm = "src/perl6/Prelude.pm";
        my $ppc_hs = "src/Pugs/PreludePC.hs";
        my $ppc_null = "src/Pugs/PreludePC.hs-null";
        if (-e $ppc_hs and -s $ppc_hs > -s $ppc_null and -M $ppc_hs < -M $pm) {
            build_lib($setup);
            build_exe($ghc, @args);
            return;
        }
    }

    run($^X, qw<util/gen_prelude.pl -v --touch --null --output src/Pugs/PreludePC.hs>);
    build_lib($setup);
    build_exe($ghc, @args);

    if (PugsBuild::Config->lookup('precompile_prelude')) {
        run($^X, qw<util/gen_prelude.pl -v -i src/perl6/Prelude.pm>,
                (map { ('-i' => $_) } @{ PugsBuild::Config->lookup('precompile_modules') }),
                '-p', $thispugs, qw<--touch --output src/Pugs/PreludePC.hs>);
        build_lib($setup);
        build_exe($ghc, @args);
    }
}

sub build_lib {
    my $setup = shift;
    system $setup, 'build', '--verbose';
    system("ar r dist/build/libHSPugs-6.2.10.a ./dist/build/src/src/Data/Yaml/Syck_stub.o");
}

sub build_exe {
    my $ghc = shift;
    my @o = qw( src/pcre/pcre.o src/syck/bytecode.o src/syck/emitter.o src/syck/gram.o src/syck/handler.o src/syck/implicit.o src/syck/node.o src/syck/syck.o src/syck/syck_st.o src/syck/token.o src/syck/yaml2byte.o src/cbits/fpstring.o src/UnicodeC.o );
    system $ghc, '--make', @_, @o, '-o' => 'pugs', 'src/Main.hs';
    #my @pkgs = qw(-package stm -package network -package mtl -package template-haskell -package base);
    # if ($^O !~ /(?:MSWin32|mingw|msys|cygwin)/) {
    #     push @pkgs, -package => 'unix';
    # }
    # system $ghc, @pkgs, qw(-idist/build -idist/build -Ldist/build -o pugs src/Main.hs -lHSPugs-6.2.10);
}

sub write_buildinfo { 
    open INFO, "> Pugs.buildinfo" or die $!;
    print INFO << ".";
ghc-options: @_
.
    if ($^O !~ /(?:MSWin32|mingw|msys|cygwin)/) {
        print INFO << ".";
build-depends: unix -any
.
    }
    close INFO;
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
