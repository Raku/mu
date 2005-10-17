#!/usr/bin/pugs

use v6;
use Test;

plan 5;

{
    my sub foo () {}

    is &foo.arity, 0;
}

{
    my sub foo ($a) {}

    is &foo.arity, 1;
}

{
    my sub foo ($a, $b) {}

    is &foo.arity, 2;
}

{
    my sub foo ($a, $b, @c) {}

    is &foo.arity, 3;
}

{
    my sub foo ($a, $b, @c, %d) {}

    is &foo.arity, 4;
}

# It's not really specced in what way (*@slurpy_params) should influence
# .arity. Also it's unclear what the result of &multisub.arity is.
# See the thread "&multisub.arity?" on p6l started by Ingo Blechschmidt for
# details:
# http://thread.gmane.org/gmane.comp.lang.perl.perl6.language/4915
