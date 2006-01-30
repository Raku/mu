#!/usr/bin/pugs

use v6;
use Test::PIL::Bootstrap;

check_pil();

my $prelude = q:to/PRELUDE/
^rFoo := ^Role.new({ '$!name' => 'rFoo' });

^rFoo.add_method('foo', -> { 'rFoo::foo' });
^rFoo.add_attribute('$foo', '$rFoo::foo');

^rBar := ^Role.new({ '$!name' => 'rBar' });

^rBar.add_method('foo', -> { 'rBar::foo' });
^rBar.add_attribute('$foo', '$rBar::foo');

^rFooBar := ^Role.new({ '$!name' => 'rFooBar' });
^rFooBar.set_roles([ ^rFoo, ^rBar ]);

^Baz := ^Class.new({ '$!name' => 'Baz' });
^Baz.set_superclasses([ ^Object ]);
^Baz.set_roles([ ^rFooBar ]);

^Baz.resolve();
PRELUDE;

pil_is_eq($prelude ~
    '^Baz.has_method("foo")',
    'false',
    '... &foo was in conflict');
    
pil_is_eq($prelude ~
    '^Baz.has_attribute("$foo")',
    'false',
    '... $foo was in conflict');    
