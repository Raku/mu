#!/usr/bin/perl

use strict;
use warnings;
use lib '../..', '../../inc';
use inc::Module::Install prefix => '../../inc';

    name('v6-pugs');
all_from('lib/v6.pm');
requires('Pugs::Compiler::Rule' => '0.06');
requires('Module::Compile' => '0.16');

recommends('Perl::Tidy' => 0);
recommends('Moose' => 0);

&WriteAll;
