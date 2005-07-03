#!/usr/bin/perl

use strict;
use warnings;

use Test::More no_plan => 1;

use Perl6::Method;

use_ok('Perl6::Util::MethodTable');


my $mt = Perl6::Util::MethodTable->new();
isa_ok($mt, 'Perl6::Util::MethodTable');

can_ok($mt, 'get_method');
can_ok($mt, 'add_method');
can_ok($mt, 'has_method');

my $foo_method = Perl6::Method->new('Foo', sub { 'Foo::foo' });
isa_ok($foo_method, 'Perl6::Method');

$@ = undef;
eval {
$mt->add_method('foo' => $foo_method);
};
ok(!$@, '... method added successfully') or diag $@;

ok($mt->has_method('foo'), '... we have the foo() method installed');

is($mt->get_method('foo'), $foo_method, '... and it is the same one we put in');

