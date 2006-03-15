#!/usr/bin/perl 

use strict;
use warnings;

use Test::More no_plan => 1;

BEGIN {
    use_ok('Pugs::Compiler::Grammar');
}

my $grammar = Pugs::Compiler::Grammar->new('Foo', '0.01');
isa_ok($grammar, 'Pugs::Compiler::Grammar');

is($grammar->_pkg_meta, Foo->meta, '... this is the same as the metaclass for Foo');

is_deeply($grammar->rules, {}, '... we have no rules at the moment');

can_ok('Foo', 'grammar');
is(Foo->grammar, $grammar, '... Foo->grammar is the same as the $grammar');

# add a rule through the meta-grammer
$grammar->add_rule('bar' => '((.).).');

{
	my $match = Foo->bar( 'abc' );
	is($match,           "abc", '... match worked as expected');
	is($match->from,     0,     '... match worked as expected');
	is($match->to,       3,     '... match worked as expected');
	is($match->[0],      "ab",  '... match worked as expected');
	is($match->[0][0],   "a",   '... match worked as expected');
}

# add a rule through the Foo package
Foo->grammar->add_rule('baz' => '((.).).');

{
	my $match = Foo->baz( 'abc' );
	is($match,           "abc", '... match worked as expected');
	is($match->from,     0,     '... match worked as expected');
	is($match->to,       3,     '... match worked as expected');
	is($match->[0],      "ab",  '... match worked as expected');
	is($match->[0][0],   "a",   '... match worked as expected');
}

