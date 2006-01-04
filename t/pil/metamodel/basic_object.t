#!./pugs

use v6;
use Test::PIL::Bootstrap;

my $prelude = q:to/PRELUDE/
^Foo := ^Class.new({});
^Foo.set_name("Foo");
^Foo.set_version("0.0.1");
^Foo.set_superclasses([ ^Object ]);
PRELUDE;

# check the Foo class

pil_is_eq($prelude ~ '^Foo`not_nil()', 'true', '... ^Foo is defined');

pil_is_eq($prelude ~ '^Foo.isa("Object")', 'true', '... ^Foo.isa(Object)');

for (qw(
    new bless CREATE BUILDALL BUILD DESTROYALL
    add_method has_method get_method get_method_list
    add_attribute has_attribute get_attribute get_attribute_list get_attributes
    superclasses set_superclasses subclasses add_subclass MRO
    dispatcher is_a isa can 
    name set_name version set_version authority set_authority identifier   
    )) -> $method_name {
    pil_is_eq($prelude ~ '^Foo.can("' ~ $method_name ~ '")', 'true', '... ^Foo.can(' ~ $method_name ~ ')');
}

pil_is_eq($prelude ~ q:to/CODE/
^Foo.name();
CODE,
'"Foo"', 
'... ^Foo.name eq Foo');

pil_is_eq($prelude ~ q:to/CODE/
^Foo.version();
CODE,
'"0.0.1"', 
'... ^Foo.version eq 0.0.1');

pil_is_eq($prelude ~ q:to/CODE/
^Foo.is_a(^Object);
CODE,
'true', 
'... ^Foo.is_a(^Object)');

pil_is_eq($prelude ~ q:to/CODE/
-> $c { $c.name() }`do_for(^Foo.MRO());
CODE,
'["Foo", "Object"]', 
'... got the right MRO for ^Foo');

## check instances

pil_is_eq($prelude ~ q:to/CODE/
$iFoo := ^Foo.new({});
$iFoo.meta()`eq(^Foo);
CODE,
'true',
'... we can create an instance of ^Foo');

for (qw/BUILDALL BUILD DESTROYALL isa can/) -> $method_name {
    pil_is_eq(
    (
        $prelude ~
        '$iFoo := ^Foo.new({});' ~ 
        '$iFoo.can("' ~ $method_name ~ '")'
    ),
    'true',
    '... $iFoo.can(' ~ $method_name ~ ')');
}

pil_is_eq($prelude ~ q:to/CODE/
$iFoo := ^Foo.new({});
$iFoo.isa("Foo");
CODE,
'true',
'... $iFoo.isa(Foo)');

pil_is_eq($prelude ~ q:to/CODE/
^Foo.add_attribute('$foo', '$Foo::foo');
$iFoo := ^Foo.new({});
$iFoo`get_attr('$foo')`eq('$Foo::foo');
CODE,
'true',
'... we can create an instance of ^Foo with attributes (with default values)');

pil_is_eq($prelude ~ q:to/CODE/
^Foo.add_attribute('$foo', '$Foo::foo');
$iFoo := ^Foo.new({ '$foo' => 'Hello, world' });
$iFoo`get_attr('$foo')`eq('Hello, world');
CODE,
'true',
'... we can create an instance of ^Foo with attributes set using BUILD');

pil_is_eq($prelude ~ q:to/CODE/
^Foo.add_attribute('$foo', '$Foo::foo');
$iFoo := ^Foo.new({ '$bar' => 'Hello, world' });
$iFoo`has_attr('$bar');
CODE,
'false',
'... un-recognized attrs are ignored by BUILD');

