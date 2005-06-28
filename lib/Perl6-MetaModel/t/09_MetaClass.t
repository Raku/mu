#!/usr/bin/perl

use strict;
use warnings;

use Test::More no_plan => 1;
use Test::Exception;

use Perl6::MetaClass;
use Perl6::Attribute;
use Perl6::Method;

my $mc= Perl6::MetaClass->new(name => 'Base');
isa_ok($mc, 'Perl6::MetaClass');

can_ok($mc, 'name');

# get direct superclasses
can_ok($mc, 'superclasses');

# get all superclasses
can_ok($mc, 'class_precedence_list');

can_ok($mc, 'add_method');

# locally defined methods
can_ok($mc, 'get_method');
can_ok($mc, 'has_method');

# methods in the class hierarchy
can_ok($mc, 'find_method');
can_ok($mc, 'find_method_in_superclasses');
can_ok($mc, 'responds_to');

# locally defined attributes
can_ok($mc, 'add_attribute');
can_ok($mc, 'get_attribute');

can_ok($mc, 'get_attribute_list');

# collect them all
can_ok($mc, 'get_all_attributes');

# now some real tests

is($mc->name, 'Base', '... got the right name for Base');

is_deeply(
    $mc->superclasses(),
    [ ], 
    '... got an empty superclasses list');

is_deeply(
    [ $mc->class_precedence_list() ],
    [ ], 
    '... got an empty class precendence list');

lives_ok {
    $mc->add_method('foo' => Perl6::Method->new($mc->name, sub { 'Base::foo' }));
} '... we can add a method successfully';

ok($mc->has_method('foo'), '... the metaclass now has the method "foo"');
ok($mc->responds_to('foo'), '... the class defined will respond to "foo"');

is($mc->get_method('foo')->call(), 'Base::foo', '... got the method and it returned the right value');
is($mc->find_method('foo')->call(), 'Base::foo', '... found the method and it returned the right value');

lives_ok {
    $mc->add_attribute('$.foo' => Perl6::Attribute->new($mc, '$.foo'));
    $mc->add_attribute('@.foo' => Perl6::Attribute->new($mc, '@.foo'))    
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
                superclasses => [ $mc ]
            );
isa_ok($mc2, 'Perl6::MetaClass');

is($mc2->name, 'Foo', '... got the right name for Foo');

is_deeply(
    $mc2->superclasses(),
    [ $mc ], 
    '... got a superclasses list');

is_deeply(
    [ $mc2->class_precedence_list() ],
    [ $mc ], 
    '... got a class precendence list');
   
lives_ok {    
    $mc2->add_method('bar' => Perl6::Method->new($mc2->name, sub { 'Foo::bar' }));
} '... add another method now';

ok($mc2->has_method('bar'), '... the metaclass now has the method "bar"');

ok($mc2->responds_to('bar'), '... the class defined will respond to "bar"');
ok($mc2->responds_to('foo'), '... the class defined will respond to "foo" (from the superclass)');

is($mc2->get_method('bar')->call(), 'Foo::bar', '... got the method and it returned the right value');
is($mc2->find_method('bar')->call(), 'Foo::bar', '... found the method and it returned the right value');

is($mc2->find_method('foo')->call(), 'Base::foo', '... found the method in the superclass and it returned the right value');

lives_ok {
    $mc2->add_attribute('$.bar' => Perl6::Attribute->new($mc2, '$.bar'));
} '... we can add attributes successfully';

ok($mc2->has_attribute('$.bar'), '... we have the attribute "$.bar"');

is_deeply(
    [ $mc2->get_all_attributes ],
    [ '$.bar', '$.foo', '@.foo' ],
    '... got the all attributes for Base');
    
isa_ok($mc2->find_attribute_spec('$.foo'), 'Perl6::Attribute');
isa_ok($mc2->find_attribute_spec('@.foo'), 'Perl6::Attribute');
isa_ok($mc2->find_attribute_spec('$.bar'), 'Perl6::Attribute');

# now add another subclasses

my $mc3 = Perl6::MetaClass->new(name => 'Bar');
isa_ok($mc3, 'Perl6::MetaClass');

is($mc3->name, 'Bar', '... got the right name for Bar');

$mc3->superclasses([ $mc ]);

is_deeply(
    $mc3->superclasses(),
    [ $mc ], 
    '... got a superclasses list');

is_deeply(
    [ $mc3->class_precedence_list() ],
    [ $mc ], 
    '... got a class precendence list');
   
lives_ok {    
    $mc3->add_method('baz' => Perl6::Method->new($mc3->name, sub { 'Bar::baz' }));
} '... add another method now';

ok($mc3->has_method('baz'), '... the metaclass now has the method "baz"');

ok($mc3->responds_to('baz'), '... the class defined will respond to "baz"');
ok($mc3->responds_to('foo'), '... the class defined will respond to "foo" (from the superclass)');

is($mc3->get_method('baz')->call(), 'Bar::baz', '... got the method and it returned the right value');
is($mc3->find_method('baz')->call(), 'Bar::baz', '... found the method and it returned the right value');

is($mc3->find_method('foo')->call(), 'Base::foo', '... found the method in the superclass and it returned the right value');

lives_ok {
    $mc3->add_attribute('$.baz' => Perl6::Attribute->new($mc3, '$.baz'));
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

is_deeply(
    $mc4->superclasses(),
    [ $mc2, $mc3 ], 
    '... got a superclasses list');

is_deeply(
    [ $mc4->class_precedence_list() ],
    [ $mc2, $mc, $mc3 ], 
    '... got a class precendence list');
   
lives_ok {    
    $mc4->add_method('blah' => Perl6::Method->new($mc4->name, sub { 'Foo::Bar::blah' }));
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

is_deeply(
    $mc5->superclasses(),
    [ $mc4 ], 
    '... got a superclasses list');

is_deeply(
    [ $mc5->class_precedence_list() ],
    [ $mc4, $mc2, $mc, $mc3 ], 
    '... got a class precendence list'); 
   
lives_ok {    
    $mc5->add_method('foo' => Perl6::Method->new($mc5->name, sub { 'Foo::Bar::Baz::foo' }));
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
