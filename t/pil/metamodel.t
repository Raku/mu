#!/usr/bin/pugs

use v6;
use Test::PIL::Bootstrap;

pil_is_eq(q:to/CODE/
::Foo := ::Class.new({});
::Foo.set_name("Foo");
::Foo.name();
CODE,
q:to/RESULT/
"Foo"
RESULT, '... created ::Foo class ok');

pil_is_eq(q:to/CODE/
::Foo := ::Class.new({});
::Foo.set_name("Foo");
::Foo.set_superclasses([ ::Object ]);
-> $c { $c.name() }.do_for(::Foo.MRO());
CODE,
q:to/RESULT/
["Foo", "Object"]
RESULT, '... got the right MRO for ::Foo');

pil_is_eq(q:to/CODE/
::Foo := ::Class.new({});
::Foo.set_name("Foo");
::Foo.set_superclasses([ ::Object ]);
$iFoo := ::Foo.new({});
$iFoo.class.eq(::Foo);
CODE,
q:to/RESULT/
true
RESULT, '... we can create an instance of ::Foo');

pil_is_eq(q:to/CODE/
::Foo := ::Class.new({});
::Foo.set_name("Foo");
::Foo.set_superclasses([ ::Object ]);
::Foo.add_attribute('$foo', '$Foo::foo');
$iFoo := ::Foo.new({});
$iFoo.get_attr('$foo').eq('$Foo::foo');
CODE,
q:to/RESULT/
true
RESULT, '... we can create an instance of ::Foo with attributes');

pil_is_eq(q:to/CODE/
::Foo := ::Class.new({});
::Foo.set_name("Foo");
::Foo.set_superclasses([ ::Object ]);
::Foo.add_attribute('$foo', '$Foo::foo');
$iFoo := ::Foo.new({}.store('$foo', 'Hello, world'));
$iFoo.get_attr('$foo').eq('Hello, world');
CODE,
q:to/RESULT/
true
RESULT, '... we can create an instance of ::Foo with attributes set using BUILD');

pil_is_eq(q:to/CODE/
::Foo := ::Class.new({});
::Foo.set_name("Foo");
::Foo.set_superclasses([ ::Object ]);
::Foo.add_attribute('$foo', '$Foo::foo');
$iFoo := ::Foo.new({}.store('$bar', 'Hello, world'));
$iFoo.has_attr('$bar');
CODE,
q:to/RESULT/
false
RESULT, '... un-recognized attrs are ignored by BUILD');



