#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 107;
use Test::Exception;

use Perl6::MetaClass;

use Perl6::Instance::Attribute;
use Perl6::Instance::Method;

use Perl6::Class::Attribute;
use Perl6::Class::Method;

my $mc= Perl6::MetaClass->new(name => 'Base');
isa_ok($mc, 'Perl6::MetaClass');

foreach my $label ('name', 'version', 'authority', 
 'identifier', 'is_a', 'superclasses', 
 'class_precedence_list', 'MRO', 'add_method', 
 'get_method', 'has_method', 'add_attribute', 
 'get_attribute', 'get_attribute_list', 'find_attribute_spec') {
     ok(::dispatch($mc, 'can', 0, $label), '... $mc->can(' . $label . ')');
}

###################################################################
# now some real tests

is(::dispatch($mc, 'name'), 'Base', '... got the right name for Base');
is(::dispatch($mc, 'version'), '0.0.0', '... got the right version for Base');
ok(!defined(::dispatch($mc, 'authority')), '... no authority for Base');

is(::dispatch($mc, 'identifier'), 'Base-0.0.0', '... got the right identifier for Base');

ok(::dispatch($mc, 'is_a', 0, ('Base')), '... the metaclass is-a Base');

is_deeply(
    ::dispatch($mc, 'superclasses'),
    [ ], 
    '... got an empty superclasses list');

is_deeply(
    [ ::dispatch($mc, 'class_precedence_list', 0, (':preorder')) ],
    [ $mc ], 
    '... got an empty class precendence list');

## class methods

lives_ok {
    ::dispatch($mc, 'add_method', 0, ('foo' => Perl6::Class::Method->new(::dispatch($mc, 'name'), sub { 'class->Base::foo' })));
} '... we can add a class method successfully';

ok(::dispatch($mc, 'has_method', 0, ('foo', for => 'Class')), '... the metaclass now has the class method "foo"');

is(::dispatch($mc, 'get_method', 0, ('foo', for => 'Class'))->do(), 'class->Base::foo', '... got the class method and it returned the right value');

## instance methods

lives_ok {
    ::dispatch($mc, 'add_method', 0, ('foo' => Perl6::Instance::Method->new(::dispatch($mc, 'name'), sub { 'Base::foo' })));
} '... we can add a method successfully';

ok(::dispatch($mc, 'has_method', 0, ('foo')), '... the metaclass now has the method "foo"');

is(::dispatch($mc, 'get_method', 0, ('foo'))->do(), 'Base::foo', '... got the method and it returned the right value');

## class attributes

lives_ok {
    ::dispatch($mc, 'add_attribute', 0, ('@.bar' => Perl6::Class::Attribute->new($mc, '@.bar')));
    ::dispatch($mc, 'add_attribute', 0, ('$:foo' => Perl6::Class::Attribute->new($mc, '$:foo')));    
} '... we can add attributes successfully';

ok(::dispatch($mc, 'has_attribute', 0, ('@.bar', for => 'Class')), '... we have the class attribute "@.bar"');
ok(::dispatch($mc, 'has_attribute', 0, ('$:foo', for => 'Class')), '... we have the class attribute "$:foo"');

is_deeply(
    [ ::dispatch($mc, 'get_attribute_list', 0, (for => 'Class')) ],
    [ '$:foo', '@.bar' ],
    '... got the right class attribute list for Base');

isa_ok(::dispatch($mc, 'find_attribute_spec', 0, ('@.bar', for => 'Class')), 'Perl6::Class::Attribute');
isa_ok(::dispatch($mc, 'find_attribute_spec', 0, ('$:foo', for => 'Class')), 'Perl6::Class::Attribute');

is_deeply(::dispatch($mc, 'get_method', 0, ('bar', for => 'Class'))->do(), [], '... our class attribute @.bar was initialized correctly');
ok(!defined(::dispatch($mc, 'find_attribute_spec', 0, ('$:foo', for => 'Class'))->get_value()), '... our class attribute $:foo was initialized correctly');

::dispatch($mc, 'find_attribute_spec', 0, ('$:foo', for => 'Class'))->set_value('class->$:foo');
is(::dispatch($mc, 'find_attribute_spec', 0, ('$:foo', for => 'Class'))->get_value(), 'class->$:foo', '... our class attribute $:foo was set correctly');

## instance attributes

lives_ok {
    ::dispatch($mc, 'add_attribute', 0, ('$.foo' => Perl6::Instance::Attribute->new($mc, '$.foo')));
    ::dispatch($mc, 'add_attribute', 0, ('@.foo' => Perl6::Instance::Attribute->new($mc, '@.foo')));   
} '... we can add attributes successfully';

ok(::dispatch($mc, 'has_attribute', 0, ('$.foo')), '... we have the attribute "$.foo"');
ok(::dispatch($mc, 'has_attribute', 0, ('@.foo')), '... we also have the attribute "@.foo"');

is_deeply(
    [ ::dispatch($mc, 'get_attribute_list') ],
    [ '$.foo', '@.foo' ],
    '... got the right attribute list for Base');

isa_ok(::dispatch($mc, 'find_attribute_spec', 0, ('$.foo')), 'Perl6::Attribute');
isa_ok(::dispatch($mc, 'find_attribute_spec', 0, ('@.foo')), 'Perl6::Attribute');

# now add subclasses

my $mc2 = Perl6::MetaClass->new(
                name         => 'Foo',
                version      => '0.0.1',
                authority    => 'http://www.foobar.com/~baz',
                superclasses => [ $mc ]
            );
isa_ok($mc2, 'Perl6::MetaClass');

is(::dispatch($mc2, 'name'), 'Foo', '... got the right name for Foo');
is(::dispatch($mc2, 'version'), '0.0.1', '... got the right version for Foo');
is(::dispatch($mc2, 'authority'), 'http://www.foobar.com/~baz', '... the correct authority for Foo');

is(::dispatch($mc2, 'identifier'), 'Foo-0.0.1-http://www.foobar.com/~baz', '... got the right identifier for Foo');

ok(::dispatch($mc2, 'is_a', 0, ('Base')), '... the metaclass is-a Base');
ok(::dispatch($mc2, 'is_a', 0, ('Foo')), '... the metaclass is-a Foo');

is_deeply(
    ::dispatch($mc2, 'superclasses'),
    [ $mc ], 
    '... got a superclasses list');

is_deeply(
    [ ::dispatch($mc2, 'class_precedence_list', 0, (':preorder')) ],
    [ $mc2, $mc ], 
    '... got a class precendence list');

lives_ok {    
    ::dispatch($mc2, 'add_method', 0, ('bar' => Perl6::Instance::Method->new(::dispatch($mc2, 'name'), sub { 'Foo::bar' })));
} '... add another method now';

ok(::dispatch($mc2, 'has_method', 0, ('bar')), '... the metaclass now has the method "bar"');

is(::dispatch($mc2, 'get_method', 0, ('bar'))->do(), 'Foo::bar', '... got the method and it returned the right value');

lives_ok {
    ::dispatch($mc2, 'add_attribute', 0, ('$.bar' => Perl6::Instance::Attribute->new($mc2, '$.bar')));
} '... we can add attributes successfully';

ok(::dispatch($mc2, 'has_attribute', 0, ('$.bar')), '... we have the attribute "$.bar"');

isa_ok(::dispatch($mc2, 'find_attribute_spec', 0, ('$.foo')), 'Perl6::Attribute');
isa_ok(::dispatch($mc2, 'find_attribute_spec', 0, ('@.foo')), 'Perl6::Attribute');
isa_ok(::dispatch($mc2, 'find_attribute_spec', 0, ('$.bar')), 'Perl6::Attribute');

is(::dispatch($mc2, 'find_attribute_spec', 0, ('$:foo', for => 'Class'))->get_value(), 'class->$:foo', '... our class attribute $:foo was set correctly');

::dispatch($mc2, 'find_attribute_spec', 0, ('$:foo', for => 'Class'))->set_value('class->$:foo again');

is(::dispatch($mc2, 'find_attribute_spec', 0, ('$:foo', for => 'Class'))->get_value(), 'class->$:foo again', '... our class attribute $:foo was set correctly');
is(::dispatch($mc, 'find_attribute_spec', 0, ('$:foo', for => 'Class'))->get_value(), 'class->$:foo again', '... our class attribute $:foo was set correctly');


# now add another subclasses

my $mc3 = Perl6::MetaClass->new(name => 'Bar');
isa_ok($mc3, 'Perl6::MetaClass');

is(::dispatch($mc3, 'name'), 'Bar', '... got the right name for Bar');

::dispatch($mc3, 'superclasses', 0, ([ $mc ]));

ok(::dispatch($mc3, 'is_a', 0, ('Base')), '... the metaclass is-a Base');
ok(::dispatch($mc3, 'is_a', 0, ('Bar')), '... the metaclass is-a Bar');

is_deeply(
    ::dispatch($mc3, 'superclasses'),
    [ $mc ], 
    '... got a superclasses list');

is_deeply(
    [ ::dispatch($mc3, 'class_precedence_list', 0, (':preorder')) ],
    [ $mc3, $mc ], 
    '... got a class precendence list');

lives_ok {    
    ::dispatch($mc3, 'add_method', 0, ('baz' => Perl6::Instance::Method->new(::dispatch($mc3, 'name'), sub { 'Bar::baz' })));
} '... add another method now';

ok(::dispatch($mc3, 'has_method', 0, ('baz')), '... the metaclass now has the method "baz"');

is(::dispatch($mc3, 'get_method', 0, ('baz'))->do(), 'Bar::baz', '... got the method and it returned the right value');

lives_ok {
    ::dispatch($mc3, 'add_attribute', 0, ('$.baz' => Perl6::Instance::Attribute->new($mc3, '$.baz')));
} '... we can add attributes successfully';

ok(::dispatch($mc3, 'has_attribute', 0, ('$.baz')), '... we have the attribute "$.bar"');

isa_ok(::dispatch($mc3, 'find_attribute_spec', 0, ('$.foo')), 'Perl6::Attribute');
isa_ok(::dispatch($mc3, 'find_attribute_spec', 0, ('@.foo')), 'Perl6::Attribute');    
isa_ok(::dispatch($mc3, 'find_attribute_spec', 0, ('$.baz')), 'Perl6::Attribute');    

# and now even more subclassing

my $mc4 = Perl6::MetaClass->new(name => 'Foo::Bar');
isa_ok($mc4, 'Perl6::MetaClass');

is(::dispatch($mc4, 'name'), 'Foo::Bar', '... got the right name for Foo::Bar');

::dispatch($mc4, 'superclasses', 0, ([ $mc2, $mc3 ]));

ok(::dispatch($mc4, 'is_a', 0, ('Base')), '... the metaclass is-a Base');
ok(::dispatch($mc4, 'is_a', 0, ('Foo')), '... the metaclass is-a Foo');
ok(::dispatch($mc4, 'is_a', 0, ('Bar')), '... the metaclass is-a Bar');
ok(::dispatch($mc4, 'is_a', 0, ('Foo::Bar')), '... the metaclass is-a Foo::Bar');

is_deeply(
    ::dispatch($mc4, 'superclasses'),
    [ $mc2, $mc3 ], 
    '... got a superclasses list');

is_deeply(
    [ ::dispatch($mc4, 'class_precedence_list', 0, (':preorder')) ],
    [ $mc4, $mc2, $mc, $mc3 ], 
    '... got a class precendence list');

lives_ok {    
    ::dispatch($mc4, 'add_method', 0, ('blah' => Perl6::Instance::Method->new(::dispatch($mc4, 'name'), sub { 'Foo::Bar::blah' })));
} '... add another method now';

ok(::dispatch($mc4, 'has_method', 0, ('blah')), '... the metaclass now has the method "blah"');

is(::dispatch($mc4, 'get_method', 0, ('blah'))->do(), 'Foo::Bar::blah', '... got the method and it returned the right value');

isa_ok(::dispatch($mc4, 'find_attribute_spec', 0, ('$.foo')), 'Perl6::Attribute');
isa_ok(::dispatch($mc4, 'find_attribute_spec', 0, ('@.foo')), 'Perl6::Attribute');
isa_ok(::dispatch($mc4, 'find_attribute_spec', 0, ('$.bar')), 'Perl6::Attribute');
isa_ok(::dispatch($mc4, 'find_attribute_spec', 0, ('$.baz')), 'Perl6::Attribute');

# and now even more-more subclassing

my $mc5 = Perl6::MetaClass->new(name => 'Foo::Bar::Baz');
isa_ok($mc5, 'Perl6::MetaClass');

is(::dispatch($mc5, 'name'), 'Foo::Bar::Baz', '... got the right name for Foo::Bar::Baz');

::dispatch($mc5, 'superclasses', 0, ([ $mc4 ]));

ok(::dispatch($mc5, 'is_a', 0, ('Base')), '... the metaclass is-a Base');
ok(::dispatch($mc5, 'is_a', 0, ('Foo')), '... the metaclass is-a Foo');
ok(::dispatch($mc5, 'is_a', 0, ('Bar')), '... the metaclass is-a Bar');
ok(::dispatch($mc5, 'is_a', 0, ('Foo::Bar')), '... the metaclass is-a Foo::Bar');
ok(::dispatch($mc5, 'is_a', 0, ('Foo::Bar::Baz')), '... the metaclass is-a Foo::Bar::Baz');

is_deeply(
    ::dispatch($mc5, 'superclasses'),
    [ $mc4 ], 
    '... got a superclasses list');

is_deeply(
    [ ::dispatch($mc5, 'class_precedence_list', 0, (':preorder')) ],
    [ $mc5, $mc4, $mc2, $mc, $mc3 ], 
    '... got a class precendence list'); 

lives_ok {    
    ::dispatch($mc5, 'add_method', 0, ('foo' => Perl6::Instance::Method->new(::dispatch($mc5, 'name'), sub { 'Foo::Bar::Baz::foo' })));
} '... add another method now';

isa_ok(::dispatch($mc5, 'find_attribute_spec', 0, ('$.foo')), 'Perl6::Attribute');
isa_ok(::dispatch($mc5, 'find_attribute_spec', 0, ('@.foo')), 'Perl6::Attribute');
isa_ok(::dispatch($mc5, 'find_attribute_spec', 0, ('$.bar')), 'Perl6::Attribute');
isa_ok(::dispatch($mc5, 'find_attribute_spec', 0, ('$.baz')), 'Perl6::Attribute');
