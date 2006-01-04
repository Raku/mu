#!./pugs

use v6;
use Test::PIL::Bootstrap;

my $prelude = q:to/PRELUDE/
^rFoo := ^Role.new({ '$!name' => 'rFoo' });

^rBar := ^Role.new({ '$!name' => 'rBar' });
^rBar.set_roles([ ^rFoo ]);

^rFooBar := ^Role.new({ '$!name' => 'rFooBar' });
^rFooBar.set_roles([ ^rFoo, ^rBar ]);

^rBaz := ^Role.new({ '$!name' => 'rBaz' });
^rBaz.set_roles([ ^rBar ]);

^Bling := ^Class.new({});
^Bling.set_roles([ ^rFooBar, ^rBaz ]);

PRELUDE;

pil_is_eq($prelude ~ '^rFoo.does("rFoo")', 'true', '... rFoo.does(rFoo)');
pil_is_eq($prelude ~ '^rFoo.does("rBoo")', 'false', '... not rFoo.does(rBoo)');

pil_is_eq($prelude ~ '^rBar.does("rBar")', 'true', '... rBar.does(rBar)');
pil_is_eq($prelude ~ '^rBar.does("rFoo")', 'true', '... rBar.does(rFoo)');
pil_is_eq($prelude ~ '^rBar.does("rBoo")', 'false', '... not rBar.does(rBoo)');

pil_is_eq($prelude ~ '^rFooBar.does("rFooBar")', 'true', '... rFooBar.does(rFooBar)');
pil_is_eq($prelude ~ '^rFooBar.does("rBar")', 'true', '... rFooBar.does(rBar)');
pil_is_eq($prelude ~ '^rFooBar.does("rFoo")', 'true', '... rFooBar.does(rFoo)');
pil_is_eq($prelude ~ '^rFooBar.does("rBoo")', 'false', '... not rFooBar.does(rBoo)');

pil_is_eq($prelude ~ '^rBaz.does("rBaz")', 'true', '... rBaz.does(rBaz)');
pil_is_eq($prelude ~ '^rBaz.does("rBar")', 'true', '... rBaz.does(rBar)');
pil_is_eq($prelude ~ '^rBaz.does("rFoo")', 'true', '... rBaz.does(rFoo)');
pil_is_eq($prelude ~ '^rBaz.does("rBoo")', 'false', '... not rBaz.does(rBoo)');

pil_is_eq($prelude ~ '^Bling.does("rFooBar")', 'true', '... Bling.does(rFooBar)');
pil_is_eq($prelude ~ '^Bling.does("rBaz")', 'true', '... Bling.does(rBaz)');
pil_is_eq($prelude ~ '^Bling.does("rBar")', 'true', '... Bling.does(rBar)');
pil_is_eq($prelude ~ '^Bling.does("rFoo")', 'true', '... Bling.does(rFoo)');
pil_is_eq($prelude ~ '^Bling.does("rBoo")', 'false', '... not Bling.does(rBoo)');

