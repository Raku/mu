use v6-alpha;

use Test;

plan 6;

sub f() { 
    my sub g(){"g"}; my sub h(){g()}; h();
};
is(f(),'g');


sub foo($x) { $x + 1 }

sub callit(&foo) {
    foo(1);
}

is(foo(1), 2);
is(callit({ $^x + 2 }), 3, "lexical subs get precedence over package subs");

sub infix:<@@> ($x, $y) { $x + $y }

sub foo2(&infix:<@@>) {
    2 @@ 3;
}

is(2 @@ 3, 5);
is(foo2({ $^a * $^b }), 6);

{
    my sub test_this {
        ok 1, "Could call ok from within a lexical sub";
        return 1;
    }
    eval 'test_this()';
    if ($!) {
        fail "Could call ok from within a lexical sub";
    }
}

# vim: ft=perl6 :
