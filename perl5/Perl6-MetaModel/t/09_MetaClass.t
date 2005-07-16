#!/usr/bin/perl

use strict;
use warnings;

use Test::More tests => 145;
use Test::Exception;

use Perl6::MetaClass;

use Perl6::Instance::Attribute;
use Perl6::Instance::Method;

use Perl6::Class::Attribute;
use Perl6::Class::Method;

my $mc= Perl6::MetaClass->new(name => 'Base');
isa_ok($mc, 'Perl6::MetaClass');

can_ok($mc, 'name');
can_ok($mc, 'version');
can_ok($mc, 'authority');
can_ok($mc, 'identifier');

can_ok($mc, 'is_a');

# get direct superclasses
can_ok($mc, 'superclasses');

# get all superclasses
can_ok($mc, 'class_precedence_list');

## Instance methods

can_ok($mc, 'add_method');

# locally defined methods
can_ok($mc, 'get_method');
can_ok($mc, 'has_method');

# methods in the class hierarchy
can_ok($mc, 'find_method');
can_ok($mc, 'find_method_in_superclasses');
can_ok($mc, 'responds_to');

## attributes

# locally defined attributes
can_ok($mc, 'add_attribute');
can_ok($mc, 'get_attribute');

can_ok($mc, 'get_attribute_list');

# collect them all
can_ok($mc, 'get_all_attributes');

can_ok($mc, 'find_attribute_spec');

###################################################################
# now some real tests

is($mc->name, 'Base', '... got the right name for Base');
is($mc->version, '0.0.0', '... got the right version for Base');
ok(!defined($mc->authority), '... no authority for Base');

is($mc->identifier, 'Base-0.0.0', '... got the right identifier for Base');

ok($mc->is_a('Base'), '... the metaclass is-a Base');

is_deeply(
    $mc->superclasses(),
    [ ], 
    '... got an empty superclasses list');

is_deeply(
    [ $mc->class_precedence_list() ],
    [ $mc ], 
    '... got an empty class precendence list');

## class methods

lives_ok {
    $mc->add_method('foo' => Perl6::Class::Method->new($mc->name, sub { 'class->Base::foo' }));
} '... we can add a class method successfully';

ok($mc->has_method('foo', for => 'Class'), '... the metaclass now has the class method "foo"');
ok($mc->responds_to('foo', for => 'Class'), '... the class defined will respond to "foo" as a class method');

is($mc->get_method('foo', for => 'Class')->call(), 'class->Base::foo', '... got the class method and it returned the right value');
is($mc->find_method('foo', for => 'Class')->call(), 'class->Base::foo', '... found the class method and it returned the right value');

## instance methods

lives_ok {
    $mc->add_method('foo' => Perl6::Instance::Method->new($mc->name, sub { 'Base::foo' }));
} '... we can add a method successfully';

ok($mc->has_method('foo'), '... the metaclass now has the method "foo"');
ok($mc->responds_to('foo'), '... the class defined will respond to "foo"');

is($mc->get_method('foo')->call(), 'Base::foo', '... got the method and it returned the right value');
is($mc->find_method('foo')->call(), 'Base::foo', '... found the method and it returned the right value');

## class attributes

lives_ok {
    $mc->add_attribute('@.bar' => Perl6::Class::Attribute->new($mc, '@.bar'));
    $mc->add_attribute('$:foo' => Perl6::Class::Attribute->new($mc, '$:foo'));    
} '... we can add attributes successfully';

ok($mc->has_attribute('@.bar', for => 'Class'), '... we have the class attribute "@.bar"');
ok($mc->has_attribute('$:foo', for => 'Class'), '... we have the class attribute "$:foo"');

is_deeply(
    [ $mc->get_attribute_list(for => 'Class') ],
    [ '$:foo', '@.bar' ],
    '... got the right class attribute list for Base');

is_deeply(
    [ $mc->get_all_attributes(for => 'Class') ],
    [ '$:foo', '@.bar' ],
    '... got the all class attributes for Base');

isa_ok($mc->find_attribute_spec('@.bar', for => 'Class'), 'Perl6::Class::Attribute');
isa_ok($mc->find_attribute_spec('$:foo', for => 'Class'), 'Perl6::Class::Attribute');

is_deeply($mc->get_method('bar', for => 'Class')->call(), [], '... our class attribute @.bar was initialized correctly');
ok(!defined($mc->find_attribute_spec('$:foo', for => 'Class')->get_value()), '... our class attribute $:foo was initialized correctly');

$mc->find_attribute_spec('$:foo', for => 'Class')->set_value('class->$:foo');
is($mc->find_attribute_spec('$:foo', for => 'Class')->get_value(), 'class->$:foo', '... our class attribute $:foo was set correctly');

## instance attributes

lives_ok {
    $mc->add_attribute('$.foo' => Perl6::Instance::Attribute->new($mc, '$.foo'));
    $mc->add_attribute('@.foo' => Perl6::Instance::Attribute->new($mc, '@.foo'))    
} '... we can add attributes successfully';

ok($mc->has_attribute('$.foo'), '... we have the attribute "$.foo"');
ok($mc->has_attribute('@.foo'), '... we also have the attribute "@.foo"');

is_deeply(
    [ $mc->get_attribute_list ],
    [ '$.foo', '@.foo' ],
    '... got the right attribute list for Base');

is_deeply(
    [ $mc->get_all_attributes ],
    [ '$.foo', '@.foo' ],
    '... got the all attributes for Base');

isa_ok($mc->find_attribute_spec('$.foo'), 'Perl6::Attribute');
isa_ok($mc->find_attribute_spec('@.foo'), 'Perl6::Attribute');

# now add subclasses

my $mc2 = Perl6::MetaClass->new(
                name         => 'Foo',
                version      => '0.0.1',
                authority    => 'http://www.foobar.com/~baz',
                superclasses => [ $mc ]
            );
isa_ok($mc2, 'Perl6::MetaClass');

is($mc2->name, 'Foo', '... got the right name for Foo');
is($mc2->version, '0.0.1', '... got the right version for Foo');
is($mc2->authority, 'http://www.foobar.com/~baz', '... the correct authority for Foo');

is($mc2->identifier, 'Foo-0.0.1-http://www.foobar.com/~baz', '... got the right identifier for Foo');

ok($mc2->is_a('Base'), '... the metaclass is-a Base');
ok($mc2->is_a('Foo'), '... the metaclass is-a Foo');

is_deeply(
    $mc2->superclasses(),
    [ $mc ], 
    '... got a superclasses list');

is_deeply(
    [ $mc2->class_precedence_list() ],
    [ $mc2, $mc ], 
    '... got a class precendence list');

lives_ok {    
    $mc2->add_method('bar' => Perl6::Instance::Method->new($mc2->name, sub { 'Foo::bar' }));
} '... add another method now';

ok($mc2->has_method('bar'), '... the metaclass now has the method "bar"');

ok($mc2->responds_to('bar'), '... the class defined will respond to "bar"');
ok($mc2->responds_to('foo'), '... the class defined will respond to "foo" (from the superclass)');

is($mc2->get_method('bar')->call(), 'Foo::bar', '... got the method and it returned the right value');
is($mc2->find_method('bar')->call(), 'Foo::bar', '... found the method and it returned the right value');

is($mc2->find_method('foo')->call(), 'Base::foo', '... found the method in the superclass and it returned the right value');

ok($mc2->responds_to('foo', for => 'Class'), '... the class defined will respond to "foo" as a class method');
is($mc2->find_method('foo', for => 'Class')->call(), 'class->Base::foo', '... found the class method and it returned the right value');

lives_ok {
    $mc2->add_attribute('$.bar' => Perl6::Instance::Attribute->new($mc2, '$.bar'));
} '... we can add attributes successfully';

ok($mc2->has_attribute('$.bar'), '... we have the attribute "$.bar"');

is_deeply(
    [ $mc2->get_all_attributes ],
    [ '$.bar', '$.foo', '@.foo' ],
    '... got the all attributes for Base');

isa_ok($mc2->find_attribute_spec('$.foo'), 'Perl6::Attribute');
isa_ok($mc2->find_attribute_spec('@.foo'), 'Perl6::Attribute');
isa_ok($mc2->find_attribute_spec('$.bar'), 'Perl6::Attribute');

is($mc2->find_attribute_spec('$:foo', for => 'Class')->get_value(), 'class->$:foo', '... our class attribute $:foo was set correctly');

$mc2->find_attribute_spec('$:foo', for => 'Class')->set_value('class->$:foo again');

is($mc2->find_attribute_spec('$:foo', for => 'Class')->get_value(), 'class->$:foo again', '... our class attribute $:foo was set correctly');
is($mc->find_attribute_spec('$:foo', for => 'Class')->get_value(), 'class->$:foo again', '... our class attribute $:foo was set correctly');


# now add another subclasses

my $mc3 = Perl6::MetaClass->new(name => 'Bar');
isa_ok($mc3, 'Perl6::MetaClass');

is($mc3->name, 'Bar', '... got the right name for Bar');

$mc3->superclasses([ $mc ]);

ok($mc3->is_a('Base'), '... the metaclass is-a Base');
ok($mc3->is_a('Bar'), '... the metaclass is-a Bar');

is_deeply(
    $mc3->superclasses(),
    [ $mc ], 
    '... got a superclasses list');

is_deeply(
    [ $mc3->class_precedence_list() ],
    [ $mc3, $mc ], 
    '... got a class precendence list');

lives_ok {    
    $mc3->add_method('baz' => Perl6::Instance::Method->new($mc3->name, sub { 'Bar::baz' }));
} '... add another method now';

ok($mc3->has_method('baz'), '... the metaclass now has the method "baz"');

ok($mc3->responds_to('baz'), '... the class defined will respond to "baz"');
ok($mc3->responds_to('foo'), '... the class defined will respond to "foo" (from the superclass)');

is($mc3->get_method('baz')->call(), 'Bar::baz', '... got the method and it returned the right value');
is($mc3->find_method('baz')->call(), 'Bar::baz', '... found the method and it returned the right value');

is($mc3->find_method('foo')->call(), 'Base::foo', '... found the method in the superclass and it returned the right value');

lives_ok {
    $mc3->add_attribute('$.baz' => Perl6::Instance::Attribute->new($mc3, '$.baz'));
} '... we can add attributes successfully';

ok($mc3->has_attribute('$.baz'), '... we have the attribute "$.bar"');

is_deeply(
    [ $mc3->get_all_attributes ],
    [ '$.baz', '$.foo', '@.foo' ],
    '... got the all attributes for Base');

isa_ok($mc3->find_attribute_spec('$.foo'), 'Perl6::Attribute');
isa_ok($mc3->find_attribute_spec('@.foo'), 'Perl6::Attribute');    
isa_ok($mc3->find_attribute_spec('$.baz'), 'Perl6::Attribute');    

# and now even more subclassing

my $mc4 = Perl6::MetaClass->new(name => 'Foo::Bar');
isa_ok($mc4, 'Perl6::MetaClass');

is($mc4->name, 'Foo::Bar', '... got the right name for Foo::Bar');

$mc4->superclasses([ $mc2, $mc3 ]);

ok($mc4->is_a('Base'), '... the metaclass is-a Base');
ok($mc4->is_a('Foo'), '... the metaclass is-a Foo');
ok($mc4->is_a('Bar'), '... the metaclass is-a Bar');
ok($mc4->is_a('Foo::Bar'), '... the metaclass is-a Foo::Bar');

is_deeply(
    $mc4->superclasses(),
    [ $mc2, $mc3 ], 
    '... got a superclasses list');

is_deeply(
    [ $mc4->class_precedence_list() ],
    [ $mc4, $mc2, $mc, $mc3 ], 
    '... got a class precendence list');

lives_ok {    
    $mc4->add_method('blah' => Perl6::Instance::Method->new($mc4->name, sub { 'Foo::Bar::blah' }));
} '... add another method now';

ok($mc4->has_method('blah'), '... the metaclass now has the method "blah"');

ok($mc4->responds_to('blah'), '... the class defined will respond to "blah"');
ok($mc4->responds_to('baz'), '... the class defined will respond to "baz" (from the superclass)');
ok($mc4->responds_to('foo'), '... the class defined will respond to "foo" (from the superclass)');

is($mc4->get_method('blah')->call(), 'Foo::Bar::blah', '... got the method and it returned the right value');

is($mc4->find_method('blah')->call(), 'Foo::Bar::blah', '... found the method and it returned the right value');

is($mc4->find_method('baz')->call(), 'Bar::baz', '... found the method and it returned the right value');
is($mc4->find_method('foo')->call(), 'Base::foo', '... found the method in the superclass and it returned the right value');

is_deeply(
    [ $mc4->get_all_attributes ],
    [ '$.bar', '$.baz', '$.foo', '@.foo' ],
    '... got the all attributes for Base');

isa_ok($mc4->find_attribute_spec('$.foo'), 'Perl6::Attribute');
isa_ok($mc4->find_attribute_spec('@.foo'), 'Perl6::Attribute');
isa_ok($mc4->find_attribute_spec('$.bar'), 'Perl6::Attribute');
isa_ok($mc4->find_attribute_spec('$.baz'), 'Perl6::Attribute');

# and now even more-more subclassing

my $mc5 = Perl6::MetaClass->new(name => 'Foo::Bar::Baz');
isa_ok($mc5, 'Perl6::MetaClass');

is($mc5->name, 'Foo::Bar::Baz', '... got the right name for Foo::Bar::Baz');

$mc5->superclasses([ $mc4 ]);

ok($mc5->is_a('Base'), '... the metaclass is-a Base');
ok($mc5->is_a('Foo'), '... the metaclass is-a Foo');
ok($mc5->is_a('Bar'), '... the metaclass is-a Bar');
ok($mc5->is_a('Foo::Bar'), '... the metaclass is-a Foo::Bar');
ok($mc5->is_a('Foo::Bar::Baz'), '... the metaclass is-a Foo::Bar::Baz');

is_deeply(
    $mc5->superclasses(),
    [ $mc4 ], 
    '... got a superclasses list');

is_deeply(
    [ $mc5->class_precedence_list() ],
    [ $mc5, $mc4, $mc2, $mc, $mc3 ], 
    '... got a class precendence list'); 

lives_ok {    
    $mc5->add_method('foo' => Perl6::Instance::Method->new($mc5->name, sub { 'Foo::Bar::Baz::foo' }));
} '... add another method now';

ok($mc5->responds_to('blah'), '... the class defined will respond to "blah"');
ok($mc5->responds_to('baz'), '... the class defined will respond to "baz" (from the superclass)');
ok($mc5->responds_to('foo'), '... the class defined will respond to "foo" (from the superclass)');

is($mc5->find_method('blah')->call(), 'Foo::Bar::blah', '... found the method and it returned the right value');
is($mc5->find_method('baz')->call(), 'Bar::baz', '... found the method and it returned the right value');

is($mc5->find_method('foo')->call(), 'Foo::Bar::Baz::foo', '... found overridden method and it returned the right value');

is($mc5->find_method_in_superclasses('foo')->call(), 'Base::foo', '... found the SUPER method and it returned the right value');

is_deeply(
    [ $mc5->get_all_attributes ],
    [ '$.bar', '$.baz', '$.foo', '@.foo' ],
    '... got the all attributes for Base');

isa_ok($mc5->find_attribute_spec('$.foo'), 'Perl6::Attribute');
isa_ok($mc5->find_attribute_spec('@.foo'), 'Perl6::Attribute');
isa_ok($mc5->find_attribute_spec('$.bar'), 'Perl6::Attribute');
isa_ok($mc5->find_attribute_spec('$.baz'), 'Perl6::Attribute');

{
    my @class_order;
    $mc5->traverse_pre_order(sub { push @class_order => $_[0] });
    is_deeply(
        \@class_order,
        [ $mc5, $mc4, $mc2, $mc, $mc3, $mc ],
        '... got the right set of metaclasses in pre-order traversal');
}

{
    my @class_order;
    $mc5->traverse_post_order(sub { push @class_order => $_[0] });
    is_deeply(
        \@class_order,
        [ $mc, $mc2, $mc, $mc3, $mc4, $mc5 ],
        '... got the right set of metaclasses in post-order traversal');
}
