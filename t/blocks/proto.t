use v6-alpha;
use Test;
plan 22;

# Prototype modifier

{ #L<S06/"Routine modifiers"/Prototypes>

    ok( proto sub foo {...}, 'proto modifier to sub' );
    ok( proto method foo {...}, 'proto modifier to method' );
    ok( proto regex foo {...}, 'proto modifier to regex' );
    ok( proto token foo {...}, 'proto modifier to token' );
    ok( proto rule foo {...}, 'proto modifier to rule' );
    ok( proto macro foo {...}, 'proto modifier to macro' );
}
