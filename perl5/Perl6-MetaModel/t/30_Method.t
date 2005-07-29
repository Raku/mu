#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 5;

use Perl6::Method;

=pod

This test file just checks the details of the Perl6::Attribute class

=cut

my $m = Perl6::Method->new('Foo' => sub { 'Foo::method' });
isa_ok($m, 'Perl6::Method');

can_ok($m, 'associated_with');
can_ok($m, 'do');

is($m->associated_with(), 'Foo', '... this method is associated with "Foo"');
is($m->do(), 'Foo::method', '... calling this method returns the expected string');
