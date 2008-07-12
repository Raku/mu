#!/usr/bin/perl

use strict;
use warnings;

my @inc;
BEGIN {
    @inc = (-e '../../inc' and -e '../../VICTUALS')
        ? (prefix => '../../inc')
        : (-e '../../../../inc' and -e '../../../../VICTUALS')
        ? (prefix => '../../../../inc')
        : ()
}
use lib '../..', (@inc ? $inc[-1] : ());
use inc::Module::Install (@inc, '.');
use File::Spec;

    name('v6');
all_from('lib/v6.pm');

requires('Pugs::Compiler::Rule' => '0.17');
requires('Module::Compile'      => '0.20');
requires('Scalar::Util'         => '0');
requires('Devel::Caller'        => '0.10');
requires('Data::Bind'           => '0.28');
requires('Sub::Multi'           => '0.003');
requires('Moose'                => '0.10');
#requires('Pugs::Emitter::Perl6::Perl5' => '0');
#requires('Lexical::Alias'       => '0');
requires('YAML::Syck'           => '0');

recommends('Perl::Tidy'         => '0');

# Parse MANIFEST and copy the tests
if (@inc) {
    use File::Copy;
    use File::Path;
    use File::Basename;

    open MANIFEST, '< MANIFEST' or die "Cannot open MANIFEST: $!";
    while (<MANIFEST>) {
        chomp;
        /^t\// or next;
        mkpath(dirname($_));
        my $src = "$inc[-1]/../$_";
        $src = "$inc[-1]/../ext/Test/lib/Test.pm" if $_ eq 't/Test.pm';
        copy($src => $_);
        `$^X -i -pe ' s/^use v6;/use v6-alpha;/ ' '$_' `
            if -f $_;
    }
}

makemaker_args  (
    test => {
        TESTS => join ' ',
            map {glob File::Spec->catfile('t', ('*') x $_, '*.t')} 0..3
    },
);

clean_files(map {glob File::Spec->catfile('t', ('*') x $_, '*.{t,pm}c')} 0..3);

&WriteAll;

# Evil hack to add PERL6LIB path to Makefile
system($^X, '-p', '-i.old', '-e', 's+(?=test_harness)+%ENV=(%ENV,qw( PERL6LIB ./t) );+g', 'Makefile');

# clean the precompiled rules cache
eval {
    require Cache::FileCache;
    my $cache = new Cache::FileCache( { 'namespace' => 'v6-rules' } );
    $cache->Clear;
};

# see Module::Compile
    # precompile Perl6Prelude.pm and Test.pm
    pmc_support();
