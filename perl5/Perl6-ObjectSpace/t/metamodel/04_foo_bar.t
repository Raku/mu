#!/usr/bin/perl

use strict;
use warnings;

use Test::More no_plan => 1;
use Test::Exception;

use_ok('Perl6::MetaModel::Bootstrap');
use_ok('Perl6::Runtime');

my $Foo = $::Class->send('new');
isa_ok($Foo, 'opaque');

is($Foo->id->greater_than(num->new(2)), $bit::TRUE, '... this is greater than the second instance');
is($Foo->class, $::Class, 'Foo is an instance of Class');

$Foo->send('superclasses' => list->new($::Object));

is($Foo->send('is_a' => $Foo), $bit::TRUE, '... Foo is_a Foo');
is($Foo->send('is_a' => $::Object), $bit::TRUE, '... Foo is_a Object');

is($Foo->send('can' => symbol->new('foo'))->is_nil, $bit::TRUE, '... !Foo->can(foo)');
is($Foo->send('has_method' => symbol->new('foo')), $bit::FALSE, '... !Foo->has_method(foo)');

my $foo = method->new(
    Perl6::Runtime::get_top_level_env(),
    closure::params->new(symbol->new('$self:' => 'opaque')),
    sub { str->new('Foo::foo') }    
);

lives_ok {
    $Foo->send('add_method' => symbol->new('foo'), $foo);
} '... added the method successfully';

is($Foo->send('has_method' => symbol->new('foo')), $bit::TRUE, '... Foo->has_method(foo)');
is($Foo->send('can' => symbol->new('foo'))->is_nil, $bit::TRUE, '... !Foo->can(foo) (we added the instance method)');

my $iFoo;
lives_ok {
    $iFoo = $Foo->send('new');
} '... create a Foo instance okay';

is($iFoo->send('can' => symbol->new('foo'))->is_nil, $bit::FALSE, '... iFoo->can(foo)');

is($iFoo->send('foo')->equal_to(str->new('Foo::foo')), $bit::TRUE, '... iFoo.foo == "Foo::foo"');



