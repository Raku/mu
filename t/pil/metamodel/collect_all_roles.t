#!./pugs

use v6;
use Test::PIL::Bootstrap;

my $prelude = q:to/PRELUDE/
::rFoo := ::Role.new({ '$!name' => 'rFoo' });

::rBar := ::Role.new({ '$!name' => 'rBar' });
::rBar.set_roles([ ::rFoo ]);

::rFooBar := ::Role.new({ '$!name' => 'rFooBar' });
::rFooBar.set_roles([ ::rFoo, ::rBar ]);

::rBaz := ::Role.new({ '$!name' => 'rBaz' });
::rBaz.set_roles([ ::rBar ]);

::Bling := ::Class.new({});
::Bling.set_roles([ ::rFooBar, ::rBaz ]);
PRELUDE;

pil_is_eq($prelude ~
'-> $r { $r.name; }`do_for(::rFoo.collect_all_roles());', 
'[]',
'... got the correct set of roles from ::rFoo');

pil_is_eq($prelude ~
'-> $r { $r.name; }`do_for(::rBar.collect_all_roles());', 
'["rFoo"]',
'... got the correct set of roles from ::rBar');

pil_is_eq($prelude ~
'-> $r { $r.name; }`do_for(::rFooBar.collect_all_roles());', 
'["rFoo", "rBar"]',
'... got the correct set of roles from ::rFooBar');

pil_is_eq($prelude ~
'-> $r { $r.name; }`do_for(::rBaz.collect_all_roles());', 
'["rBar", "rFoo"]',
'... got the correct set of roles from ::rBaz');

pil_is_eq($prelude ~
'-> $r { $r.name; }`do_for(::Bling.collect_all_roles());', 
'["rFooBar", "rFoo", "rBar", "rBaz"]',
'... got the correct set of roles from ::Bling');
