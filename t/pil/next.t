#!/usr/bin/pugs

use v6;
use Test::PIL::Bootstrap;

my $classes = q:to/CLASSES/

::Foo := ::Class.new({});
::Foo.set_superclasses([ ::Object ]);
::Foo.add_method('foo', -> $x { 
    'Foo::foo('.concat($x).concat(')') 
});

::Bar := ::Class.new({});
::Bar.set_superclasses([ ::Foo ]);
::Bar.add_method('foo', -> $x { 
    'Bar::foo('.concat($x).concat(') -> ').concat(&?NEXT.($x)) 
});

::Baz := ::Class.new({});
::Baz.set_superclasses([ ::Bar ]);
::Baz.add_method('foo', -> $x { 
    'Baz::foo('.concat($x).concat(') -> ').concat(&?NEXT.($x)) 
});

CLASSES;

pil_is_eq($classes ~ q:to/CODE/
$bar := ::Bar.new({});
$bar.foo("3");
CODE,
'"Bar::foo(3) -> Foo::foo(3)"',
'... &?NEXT chained through 2 classes');

pil_is_eq($classes ~ q:to/CODE/
$baz := ::Baz.new({});
$baz.foo("3");
CODE,
'"Baz::foo(3) -> Bar::foo(3) -> Foo::foo(3)"',
'... &?NEXT chained through 3 classes');

# a variation on the above

my $classes2 = q:to/CLASSES2/

::Foo := ::Class.new({});
::Foo.set_superclasses([ ::Object ]);
::Foo.add_method('foo', -> $x { 
    'Foo::foo('.concat($x).concat(')') 
});

::Bar := ::Class.new({});
::Bar.set_superclasses([ ::Foo ]);
::Bar.add_method('foo', -> $x { 
    'Bar::foo('.concat($x).concat(') -> ').concat(&?NEXT.($x.increment())) 
});

::Baz := ::Class.new({});
::Baz.set_superclasses([ ::Bar ]);
::Baz.add_method('foo', -> $x { 
    'Baz::foo('.concat($x).concat(') -> ').concat(&?NEXT.($x.increment())) 
});

CLASSES2;

pil_is_eq($classes2 ~ q:to/CODE/
$bar := ::Bar.new({});
$bar.foo(3);
CODE,
'"Bar::foo(3) -> Foo::foo(4)"',
'... &?NEXT chained through 2 classes (with new param)');

pil_is_eq($classes2 ~ q:to/CODE/
$baz := ::Baz.new({});
$baz.foo(3);
CODE,
'"Baz::foo(3) -> Bar::foo(4) -> Foo::foo(5)"',
'... &?NEXT chained through 3 classes (with new param)');


