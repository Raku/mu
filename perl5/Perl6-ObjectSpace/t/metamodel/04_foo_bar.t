#!/usr/bin/perl

use strict;
use warnings;

use Test::More no_plan => 1;
use Test::Exception;

use_ok('Perl6::MetaModel::Bootstrap');
use_ok('Perl6::Runtime');

my $Foo = $::Class->send('new' => hash->new(
                            str->new('$:name')    => str->new('Foo'),
                            str->new('$:version') => str->new('0.0.1')                                
                        ));
isa_ok($Foo, 'opaque');

is($Foo->id->greater_than(num->new(2)), $bit::TRUE, '... this is greater than the second instance');
is($Foo->class, $::Class, 'Foo is an instance of Class');

$Foo->send('superclasses' => list->new($::Object));

is($Foo->send('is_a' => $Foo), $bit::TRUE, '... Foo is_a Foo');
is($Foo->send('is_a' => $::Object), $bit::TRUE, '... Foo is_a Object');

is($Foo->send('can' => symbol->new('foo'))->is_nil, $bit::TRUE, '... !Foo->can(foo)');
is($Foo->send('has_method' => symbol->new('foo')), $bit::FALSE, '... !Foo->has_method(foo)');

is($Foo->send('name')->equal_to(str->new('Foo')), $bit::TRUE, '... Foo.name == Foo');
is($Foo->send('version')->equal_to(str->new('0.0.1')), $bit::TRUE, '... Foo.version == 0.0.1');
is($Foo->send('authority')->to_bit(), $bit::FALSE, '... Foo.authority == NIL');

is($Foo->send('identifier')->equal_to(str->new('Foo-0.0.1')), $bit::TRUE, '... Foo.identifier == "Foo-0.0.1"');

my $foo = method->new(
    Perl6::Runtime::get_top_level_env(),
    method::params->new(symbol->new('$self:' => 'opaque')),
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


# -----------------------------------------------------------------------------

my $Bar = $::Class->send('new' => hash->new(
                            str->new('$:name')      => str->new('Bar'),
                            str->new('$:version')   => str->new('0.2.1'),                              
                            str->new('$:authority') => str->new('cpan:JRANDOM')                                                            
                        ));
isa_ok($Bar, 'opaque');

$Bar->send('superclasses' => list->new($Foo));

is($Bar->id->greater_than($Foo->id), $bit::TRUE, '... this is greater than the second instance');
is($Bar->class, $::Class, 'Bar is an instance of Class');

is($Bar->send('is_a' => $Bar), $bit::TRUE, '... Bar is_a Bar');
is($Bar->send('is_a' => $Foo), $bit::TRUE, '... Bar is_a Foo');
is($Bar->send('is_a' => $::Object), $bit::TRUE, '... Bar is_a Object');

is($Bar->send('has_method' => symbol->new('foo')), $bit::FALSE, '... !Bar->has_method(foo)');
is($Bar->send('can' => symbol->new('foo'))->is_nil, $bit::TRUE, '... Bar->can(foo) (from Foo)');

is($Bar->send('name')->equal_to(str->new('Bar')), $bit::TRUE, '... Bar.name == Bar');
is($Bar->send('version')->equal_to(str->new('0.2.1')), $bit::TRUE, '... Bar.version == 0.2.1');
is($Bar->send('authority')->equal_to(str->new('cpan:JRANDOM')), $bit::TRUE, '... Bar.authority == cpan:JRANDOM');

is($Bar->send('identifier')->equal_to(str->new('Bar-0.2.1-cpan:JRANDOM')), $bit::TRUE, '... Bar.identifier == "Bar-0.2.1-cpan:JRANDOM"');

my $iBar;
lives_ok {
    $iBar = $Bar->send('new');
} '... create a Bar instance okay';

is($iBar->send('foo')->equal_to(str->new('Foo::foo')), $bit::TRUE, '... iBar.foo == "Foo::foo"');
