use Test;
is(do {
    my sub foo($x) {
        $x+2
    }
    foo(7);
},9);
done_testing;
