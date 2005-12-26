#!./pugs

use v6;
use Test::PIL::Bootstrap;

my $prelude = q:to/PRELUDE/
::rFoo := ::Role.new({ '$!name' => 'rFoo' });

::rFoo.add_method('foo', -> { 'rFoo::foo' });
::rFoo.add_attribute('$foo', '$rFoo::foo');

::rBar := ::Role.new({ '$!name' => 'rBar' });
::rBar.set_roles([ ::rFoo ]);

::rBar.add_method('bar', -> { 'rBar::bar' });
::rBar.add_attribute('$bar', '$rBar::bar');

::rFooBar := ::Role.new({ '$!name' => 'rFooBar' });
::rFooBar.set_roles([ ::rFoo, ::rBar ]);

::rFooBar.add_method('foo_bar', -> { 'rFooBar::foo_bar' });
::rFooBar.add_attribute('$foo_bar', '$rFooBar::foo_bar');

::rBaz := ::Role.new({ '$!name' => 'rBaz' });
::rBaz.set_roles([ ::rBar ]);

::rBaz.add_method('baz', -> { 'rBaz::baz' });
::rBaz.add_attribute('$baz', '$rBaz::baz');

::Bling := ::Class.new({ '$!name' => 'Bling' });
::Bling.set_superclasses([ ::Object ]);
::Bling.set_roles([ ::rFooBar, ::rBaz ]);

::Bling.resolve();
PRELUDE;

for qw/foo bar foo_bar baz/ -> $method_name {
    pil_is_eq($prelude ~ 
        '::Bling.has_method("' ~ $method_name ~ '")', 
        'true', 
        '... ::Bling.has_method(' ~ $method_name ~ ')'
    );  
}

for qw/$foo $bar $foo_bar $baz/ -> $attribute_name {
    pil_is_eq($prelude ~ 
        '::Bling.has_attribute("' ~ $attribute_name ~ '")', 
        'true', 
        '... ::Bling.has_attribute(' ~ $attribute_name ~ ')'
    );  
}

for qw/rFoo rBar rFooBar rBaz/ -> $role {
    pil_is_eq($prelude ~ 
        '::Bling.new({}).does("' ~ $role ~ '")', 
        'true', 
        '... ::Bling.new().does(' ~ $role ~ ')');
}

pil_is_eq($prelude ~ '$bling := ::Bling.new({}); $bling.foo()', '"rFoo::foo"', '... $bling.foo == rFoo::foo');
pil_is_eq($prelude ~ '$bling := ::Bling.new({}); $bling.bar()', '"rBar::bar"', '... $bling.bar == rBar::bar');
pil_is_eq($prelude ~ 
    '$bling := ::Bling.new({}); $bling.foo_bar()', 
    '"rFooBar::foo_bar"', 
    '... $bling.foo_bar == rFooBar::foo_bar');
pil_is_eq($prelude ~ '$bling := ::Bling.new({}); $bling.baz()', '"rBaz::baz"', '... $bling.baz == rBaz::baz');


pil_is_eq($prelude ~ 
    '$bling := ::Bling.new({}); $bling`get_attr("$foo")', 
    '"$rFoo::foo"', 
    '... $bling.$foo == $rFoo::foo');

pil_is_eq($prelude ~ 
    '$bling := ::Bling.new({}); $bling`get_attr("$bar")', 
    '"$rBar::bar"', 
    '... $bling.$bar == $rBar::bar');
    
pil_is_eq($prelude ~ 
    '$bling := ::Bling.new({}); $bling`get_attr("$foo_bar")', 
    '"$rFooBar::foo_bar"', 
    '... $bling.$foo_bar == $rFooBar::foo_bar');
    
pil_is_eq($prelude ~ 
    '$bling := ::Bling.new({}); $bling`get_attr("$baz")', 
    '"$rBaz::baz"', 
    '... $bling.baz == $rBaz::baz');

