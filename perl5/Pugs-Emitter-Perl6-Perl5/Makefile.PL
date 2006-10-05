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

    name('Pugs-Emitter-Perl6-Perl5');
# TODO
# all_from('lib/v6.pm');

# Pugs::Runtime::Common
requires('Pugs::Compiler::Rule' => '0.17');
requires('Module::Compile'      => '0.17');
requires('Scalar::Util'         => '0');

&WriteAll;
