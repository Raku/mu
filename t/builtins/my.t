#!/usr/bin/pugs

use v6;
use Test;

plan 13;

# basic my() lexicals

my $a = 1;
ok($a, '$a is available in this scope');

if (1) { # create a new lexical scope
    ok($a, '$a is available in this scope');
    my $b = 1;
    ok($b, '$b is available in this scope');
}
ok(!(eval'$b'), '$b is not available in this scope');

# changing a lexical within a block retains the changed value

my $c = 1;
if (1) { # create a new lexical scope
    is($c, 1, '$c is still the same outer value');
    $c = 2;
}
is($c, 2, '$c is available, and the outer value has been changed');

# shadowing a lexical with a new lexical of the same name
# and that lexical does not leak out into the outer scope

my $d = 1;
if (1) { # create a new lexical scope
    is($d, 1, '$d is still the outer $d');
    my $d = 2;
    is($d, 2, '$d is now the lexical (inner) $d');    
}
is($d, 1, '$d is available, and the outer value has not changed');

# check closures with functions

my $func;
my $func2;
if (1) { # create a new lexical scope
    my $e = 0;
    $func = sub { $e++ }; # one to inc
    $func2 = sub { $e };  # one to access it
}

ok(!(eval'$e'), '$e is the not available in this scope');
is($func2(), 0, '$func2() just returns the $e lexical which is held by the closure');
$func();
is($func2(), 1, '$func() increments the $e lexical which is held by the closure');
$func();
is($func2(), 2, '... and one more time just to be sure');
