use v6-alpha;
use Test;
plan 6;

# Prototype modifier

{ #L<S06/"Routine modifiers"/Prototypes>

    eval q[ proto sub       f1 {...} ];
    eval q[ proto method    f2 {...} ];
    eval q[ proto macro     f3 {...} ];
    eval q[ proto regex     f4 {...} ];
    eval q[ proto token     f5 {...} ];
    eval q[ proto rule      f6 {...} ];

    ok( eval('&f1'), 'proto modifier to sub'                 );
    ok( eval('&f2'), 'proto modifier to method'              );
    ok( eval('&f3'), 'proto modifier to macro'               );
    ok( eval('&f4'), 'proto modifier to regex', :todo<bug>   );
    ok( eval('&f5'), 'proto modifier to token', :todo<bug>   );
    ok( eval('&f6'), 'proto modifier to rule',  :todo<bug>   );
}
