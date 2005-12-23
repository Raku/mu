#!./pugs

use v6;
use Test::PIL::Bootstrap;

my $prelude = q:to/PRELUDE/
::rFoo := ::Role.new({ '$!name' => 'rFoo' });

::rFoo.add_method('foo', -> { 'rFoo::foo' });

::rBar := ::Role.new({ '$!name' => 'rBar' });
::rBar.set_roles([ ::rFoo ]);

::rBar.add_method('bar', -> { 'rBar::bar' });

::rFooBar := ::Role.new({ '$!name' => 'rFooBar' });
::rFooBar.set_roles([ ::rFoo, ::rBar ]);

::rFooBar.add_method('foo_bar', -> { 'rFooBar::foo_bar' });

::rBaz := ::Role.new({ '$!name' => 'rBaz' });
::rBaz.set_roles([ ::rBar ]);

::rBaz.add_method('baz', -> { 'rBaz::baz' });

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

for qw/rFoo rBar rFooBar rBaz/ -> $role {
    pil_is_eq($prelude ~ 
        '::Bling.new({}).does("' ~ $role ~ '")', 
        'true', 
        '... ::Bling.new().does(' ~ $role ~ ')');
}


