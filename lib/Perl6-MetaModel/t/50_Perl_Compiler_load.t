#!/usr/bin/perl

use strict;
use warnings;

use Test::More no_plan => 1;

use lib 't/lib';

use_ok('Perl::Compiler::PIL');
use_ok('Perl::Compiler::PIL::Util');
