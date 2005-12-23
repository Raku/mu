#!./pugs

use v6;
use Test::PIL::Bootstrap;

my $prelude = q:to/PRELUDE/
::Foo := ::Class.new({});
::Foo.set_name("Foo");
::Foo.set_version("0.0.1");
::Foo.set_superclasses([ ::Object ]);
::Foo.add_attribute('$foo', '$foo');
::Foo.add_method('foo', -> { 'Foo::foo' });
::Bar := ::Class.new({});
::Bar.set_name("Bar");
::Bar.set_version("0.0.1");
::Bar.set_superclasses([ ::Foo ]);
::Bar.add_attribute('$bar', '$bar');
::Bar.add_method('bar', -> { 'Bar::bar' });
PRELUDE;

pil_is_eq($prelude ~ '::Foo.not_nil()', 'true', '... ::Foo is defined');
pil_is_eq($prelude ~ '::Bar.not_nil()', 'true', '... ::Bar is defined');

# check the classes

pil_is_eq($prelude ~ '::Bar.is_a(::Foo)', 'true', '... ::Bar.is_a(::Foo)');

pil_is_eq($prelude ~ '::Foo.has_method("foo")', 'true', '... ::Foo.has_method(foo)');
pil_is_eq($prelude ~ '::Bar.has_method("bar")', 'true', '... ::Bar.has_method(bar)');
pil_is_eq($prelude ~ '::Bar.has_method("foo")', 'false', '... ::Bar.has_method(foo) == false');

pil_is_eq($prelude ~ '::Foo.has_attribute("$foo")', 'true', '... ::Foo.has_attribute($foo)');
pil_is_eq($prelude ~ '::Bar.has_attribute("$bar")', 'true', '... ::Bar.has_attribute($bar)');
pil_is_eq($prelude ~ '::Bar.has_attribute("$foo")', 'false', '... ::Bar.has_attribute($foo) == false');

pil_is_eq($prelude ~ '::Foo.isa("Object")', 'true', '... ::Foo.isa(Object)');
pil_is_eq($prelude ~ '::Bar.isa("Object")', 'true', '... ::Bar.isa(Object)');
pil_is_eq($prelude ~ '::Bar.isa("Foo")', 'true', '... ::Bar.isa(Foo)');

# check the objects

pil_is_eq($prelude ~ '::Foo.new({}).isa("Foo")', 'true', '... ::Foo.new.isa(Foo)');
pil_is_eq($prelude ~ '::Foo.new({}).isa("Object")', 'true', '... ::Foo.new.isa(Object)');

pil_is_eq($prelude ~ '::Bar.new({}).isa("Bar")', 'true', '... ::Bar.new.isa(Bar)');
pil_is_eq($prelude ~ '::Bar.new({}).isa("Foo")', 'true', '... ::Bar.new.isa(Foo)');
pil_is_eq($prelude ~ '::Bar.new({}).isa("Object")', 'true', '... ::Bar.new.isa(Object)');

pil_is_eq($prelude ~ '::Foo.new({}).can("foo")', 'true', '... ::Foo.new.can(foo)');
pil_is_eq($prelude ~ '::Bar.new({}).can("foo")', 'true', '... ::Bar.new.can(foo)');

pil_is_eq($prelude ~ '::Bar.new({}).can("bar")', 'true', '... ::Bar.new.can(bar)');

pil_is_eq($prelude ~ '::Foo.new({}).can("bar")', 'false', '... ::Foo.new.can(bar) == false');

pil_is_eq($prelude ~ '::Foo.new({}).foo()', '"Foo::foo"', '... ::Foo.new.foo() is Foo::foo');
pil_is_eq($prelude ~ '::Bar.new({}).foo()', '"Foo::foo"', '... ::Bar.new.foo() is Foo::foo');
pil_is_eq($prelude ~ '::Bar.new({}).bar()', '"Bar::bar"', '... ::Bar.new.bar() is Bar::bar');

pil_is_eq($prelude ~ '::Foo.new({}).has_attr("$foo")', 'true', '... ::Foo.new.has_attr($foo)');
pil_is_eq($prelude ~ '::Bar.new({}).has_attr("$foo")', 'true', '... ::Bar.new.has_attr($foo)');
pil_is_eq($prelude ~ '::Bar.new({}).has_attr("$bar")', 'true', '... ::Bar.new.has_attr($bar)');

pil_is_eq($prelude ~ '::Foo.new({}).has_attr("$bar")', 'false', '... ::Foo.new.has_attr($bar) == false');

pil_is_eq(
$prelude ~ '::Foo.new({ "$foo" => "testing $foo" }).get_attr("$foo")', 
'"testing $foo"', 
'... ::Foo.new.get_attr($foo) == "testing $foo"');

pil_is_eq(
$prelude ~ '::Bar.new({ "$foo" => "testing $foo" }).get_attr("$foo")', 
'"testing $foo"', 
'... ::Bar.new.get_attr($foo) == "testing $foo"');

pil_is_eq(
$prelude ~ '::Bar.new({ "$bar" => "testing $bar" }).get_attr("$bar")', 
'"testing $bar"', 
'... ::Bar.new.get_attr($bar) == "testing $bar"');

pil_is_eq($prelude ~ q:to/CODE/
$bar := ::Bar.new({ "$foo" => "testing $foo", "$bar" => "testing $bar" });
[ $bar.get_attr("$foo"), $bar.get_attr("$bar") ]
CODE,
'["testing $foo", "testing $bar"]', 
'... ::Bar.new.get_attr($bar) & .get_attr($foo)');

