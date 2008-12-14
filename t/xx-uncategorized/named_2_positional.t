use v6;

use Test;

plan 2;

# Named parameters shouldn't be able to automatically morph
# into positionals.  IOW named( foo => 3 ) ne named( 3 )

sub named ( :$foo ) { $foo }

is named(foo => 'bar'),'bar', 'Named parameters work when passed as named';
dies_ok { named('bar') }, "Named parameters don't work as positionals";
