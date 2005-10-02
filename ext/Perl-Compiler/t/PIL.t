#!/usr/bin/pugs

use v6;
use Test;

plan 2;

use_ok('Perl::Compiler::PIL');

{   # 1 = 2 breaks
my $literal_1 = ::Perl::Compiler::PIL::PILVal.new(value => 1);
my $literal_2 = ::Perl::Compiler::PIL::PILVal.new(value => 2);
dies_ok({ ::Perl::Compiler::PIL::PILAssign.new(lefts => [$literal_1], right => $literal_2) }, '1 = 2 fails');
}

# vim: ft=perl6 :
