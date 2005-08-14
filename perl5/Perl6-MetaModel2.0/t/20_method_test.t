#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 3;
use Test::Exception;

BEGIN { do "lib/chaos.pl" };

my $method;
lives_ok {
    $method = ::instance_method {
        return "Hello World";
    } 'Foo';
} '... constructed the instance method okay';
isa_ok($method, 'Perl6::Instance::Method');

is($method->do(), 'Hello World', '... got the right return value');