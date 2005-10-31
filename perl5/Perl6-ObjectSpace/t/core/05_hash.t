#!/usr/bin/perl

use strict;
use warnings;

use Test::More no_plan => 1;

use_ok('Perl6::Core::Hash');

my $hash = hash->new();
isa_ok($hash, 'hash');