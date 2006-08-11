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
use inc::Module::Install @inc;

        name('v6-Compiler');
version_from('lib/v6/Compiler.pmc') if -e 'lib/v6/Compiler.pmc';
    all_from('lib/v6/Compiler.pm');

# requires('v6' => '0.010');

clean_files('t/*.tc', 't/*.pmc', 't/*/*.tc', 't/*/*.pmc', 't/*/*/*.tc', 't/*/*/*.pmc', 'lib/*.pmc', 'lib/*/*.pmc', 'lib/*/*/*.pmc');

&WriteAll;

# see Module::Compile
    pmc_support();

__END__

TODO ...

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
        $src = "../../ext/Test/lib/Test.pm" if $_ eq 't/Test.pm';
        copy($src => $_);
    }
}

makemaker_args  (
    test => { TESTS => join ' ', "t/*.t", "t/*/*.t", "t/*/*/*.t" },
);

&WriteAll;

# Evil hack to add PERL6LIB path to Makefile
system($^X, '-p', '-i.old', '-e', 's+(?=test_harness)+%ENV=(%ENV,qw( PERL6LIB ./t) );+g', 'Makefile');
