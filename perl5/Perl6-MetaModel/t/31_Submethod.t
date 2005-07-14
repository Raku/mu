#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 8;
use Test::Exception;

use Perl6::MetaModel;
use Perl6::SubMethod;

class Foo => {};

Foo->meta->add_method('bar' => Perl6::SubMethod->new('Foo', sub { 'Foo::bar<submethod>' }));

my $foo = Foo->new();
isa_ok($foo, 'Foo');

can_ok($foo, 'bar');

{
    my $value;
    lives_ok {
        $value = $foo->bar()
    } '... calling bar() succedded';
    is($value, 'Foo::bar<submethod>', '... got the right return value');    
}

class Baz => {
    is => [ 'Foo' ]
};

my $baz = Baz->new();
isa_ok($baz, 'Baz');
isa_ok($baz, 'Foo');

can_ok($baz, 'bar');

dies_ok {
    $baz->bar()
} '... calling bar() failed';

