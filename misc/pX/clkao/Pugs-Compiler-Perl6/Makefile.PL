#!/usr/bin/perl

use strict;
use warnings;
my $inc;
BEGIN {
$inc = -e '../../inc' ? '../../inc' : '../../../../inc';
}
use lib '../..', $inc;
use inc::Module::Install prefix => $inc;

    name('v6-pugs');
all_from('lib/v6.pm');
requires('Pugs::Compiler::Rule' => '0.06');
requires('Module::Compile' => '0.16');
requires('Scalar::Util' => 0);
requires('Devel::Caller' => 0);
requires('Data::Bind' => 0);

recommends('Perl::Tidy' => 0);
recommends('Moose' => 0);

clean_files('t/*.tc', 't/*.pmc');

&WriteAll;
