#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 4;

use_ok('Perl6::Container::Scalar');

my $s = Perl6::Container::Scalar->new();
isa_ok($s, 'Perl6::Container::Scalar');

is($s->CONST, 0, '... got the right value from CONST');

$s->STORE(42);

is($s->FETCH(), 42, '... got the right value');
