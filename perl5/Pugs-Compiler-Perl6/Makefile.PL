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

    name('v6-alpha');
all_from('lib/v6.pm');

requires('Pugs::Compiler::Rule' => '0.06');
requires('Module::Compile'      => '0.17');
requires('Scalar::Util'         => '0');
requires('Devel::Caller'        => '0.10');
requires('Data::Bind'           => '0.25');
requires('Moose'                => '0.10');

recommends('Perl::Tidy'         => '0');

clean_files('t/*.tc', 't/*.pmc');

&WriteAll;

# Evil hack to add PERL6LIB path to Makefile
if (@inc) {
    system($^X, '-p', '-i.old', '-e', 's+(?=test_harness)+%ENV=(%ENV,qw( PERL6LIB ../../ext/Test/lib) );+g', 'Makefile');
}
else {
    system($^X, '-p', '-i.old', '-e', 's+(?=test_harness)+%ENV=(%ENV,qw( PERL6LIB ./t) );+g', 'Makefile');
}
