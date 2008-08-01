#!/usr/bin/perl

use 5.006;
use strict;
use Cwd;
use Config;
use ExtUtils::Embed;

my $embed_flags = "-I" . cwd();
my $ccdlflags = "";
my $flags = "$Config{ccflags} $Config{ccdlflags} ";

if ($flags =~ /\S/) {
    $flags =~ s{([\\"'])}{\\$1}g;
    my @flags = grep { length $_ } split /\s+/, $flags;

    if ($^O eq 'MSWin32') {
        if ($Config{libperl} =~ /lib(\w+)\.a/) {
            $embed_flags .= " -optl-l$1 ";
        }
        elsif (defined &Win32::BuildNumber) {
            # We are on ActivePerl -- Kluge massively!

            no warnings 'once';
            our %MY_CONFIG = %Config;
            *Config = *MY_CONFIG;
            *Config::Config = *MY_CONFIG;
            *ExtUtils::MM_Win32::Config = *MY_CONFIG;
            *ExtUtils::MM_Unix::Config = *MY_CONFIG;

            $Config{ccflags} =~ s/-libpath:"?(.*?)"? //g;
            $Config{ccdlflags} =~ s/-libpath:"?(.*?)"? //g;
            $Config{lddlflags} =~ s/-libpath:"?(.*?)"? //g;
            $Config{ldflags} =~ s/-libpath:"?(.*?)"? //g
                or die "ldflags: $Config{ldflags} does not contain -libpath:";

            my $lib = "$1/$Config{libperl}";
            $embed_flags .= " -optl\"$lib\" ";

            $flags = "$Config{ccflags} $Config{ccdlflags}";
            $flags =~ s{([\\"'])}{\\$1}g;
            @flags = grep { length $_ } split /\s+/, $flags;
        }
        else {
            warn "Unrecognized libperl shared library: $Config{libperl}, proceeding anyway...\n";
        }

        $ccdlflags .= (/^-[DIL]/ ? ' -optc' : ' -optl') . qq["$_" ] for @flags;
        $embed_flags .= " -optc-Ddirent=DIRENT";
    }
    else {
        $embed_flags .= " -optc$_" for grep length, split(/\s+/, ccopts());
        $embed_flags .= " -optl$_" for grep length, split(/\s+/, ldopts());
    }

    $embed_flags .= " $_" for grep { /-[DIL]/ } split(/\s+/, ccopts());
    $embed_flags .= " $_" for grep { /-[DIL]/ } split(/\s+/, ldopts());

    if ($Config{osname} eq 'cygwin') {
        my $cygpath = sub {
            my $path = `cygpath -m @_`;
            chomp $path;
            return $path;
        };
        $embed_flags =~ s{(/usr/\S+)}{$cygpath->($1)}eg;
        $embed_flags =~ s{/cygdrive/(\w)/}{$1:/}g;
        #warn "** Cygwin embedding flags: embed_flags\n";
    }
}

my @include_dirs = split(/\s+/, perl_inc());
s/^-I// for @include_dirs;

my @cc_options = map { /^-optc(.+)/ ? $1 : () } (split(/\s+/, $embed_flags), split(/\s+/, $ccdlflags));
my @ld_options = grep { not /,/ } map { /^-optl(.+)/ ? $1 : () } (split(/\s+/, $embed_flags), split(/\s+/, $ccdlflags));

open INFO, ">Pugs.buildinfo" or die "Cannot write build info: $!";
my $info = << ".";
executable: pugs
ghc-options: $embed_flags $ccdlflags
include-dirs: @include_dirs
cc-options: @cc_options
ld-options: @ld_options
.
$info =~ s/-arch\s+\w+\s*//g;
print INFO $info;
close INFO;
