#!/usr/bin/perl 

use strict;
use warnings;

use Test::More no_plan => 1;

BEGIN {
    use_ok('Pugs::Compiler::Rule');
}

my $rule = Pugs::Compiler::Rule->compile( '((.).).' );
isa_ok($rule, 'Pugs::Compiler::Rule');

my $match = $rule->match( 'abc' );
isa_ok($match, 'Pugs::Runtime::Match');

is($match,           "abc", '... match worked as expected');
is($match->from,     0,     '... match worked as expected');
is($match->to,       3,     '... match worked as expected');
is($match->[0],      "ab",  '... match worked as expected');
is($match->[0][0],   "a",   '... match worked as expected');