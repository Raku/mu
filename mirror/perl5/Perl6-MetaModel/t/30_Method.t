#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 3;

use Perl6::Method;

=pod

This test file just checks the details of the Perl6::Attribute class

=cut

my $m = Perl6::Method->create_instance_method('Foo' => sub { 'Foo::method' });
isa_ok($m, 'Perl6::Method');

can_ok($m, 'do');
is($m->do(), 'Foo::method', '... calling this method returns the expected string');
