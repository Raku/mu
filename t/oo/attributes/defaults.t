#!/usr/bin/pugs

use v6;
use Test;

plan 15;

# L<S12/"Attributes" /The value on the right is evaluated at class composition/>

my $got_a_num;  sub get_a_num  { $got_a_num++;  42 }
my $got_a_str;  sub get_a_str  { $got_a_str++;  "Pugs" }

my $got_a_code;
my $was_in_closure;
sub get_a_code {
    $got_a_code++;
    return {
        $was_in_closure++;
        42;
    };
}

eval 'class Foo {
    has $.num  = get_a_num();
    has $.str  = { get_a_str() };
    has $.code = { get_a_code() };
}';

{
    is $got_a_num, 1, "default should be called at compile-time";
    my Foo $foo .= new;
    is $got_a_num, 1, "default should be called only once, at compile-time (1)";
    is $foo.num,  42, "attribute default worked";
    is $got_a_num, 1, "default should be called only once, at compile-time (2)";
}

{
    $got_a_str = 0;  # reset

    {
        my Foo $foo .= new;
        is $got_a_str,            1, "using a coderef as a default value delays execution";
        is try { $foo.str }, "Pugs", "attribute default worked";
    }

    {
        my Foo $foo .= new;
        is $got_a_str,            2, "using a coderef as a default value delays execution";
        is try { $foo.str }, "Pugs", "attribute default worked";
    }
}

{
    $got_a_code = 0;  # reset

    {
        my Foo $foo .= new;
        is $got_a_code,     1, "using a coderef as a default value delays execution";
        is $was_in_closure, 0, "sub-coderef not yet executed";
        try { $foo.code };
        is $was_in_closure, 0, "sub-coderef still not executed";
    }

    {
        my Foo $foo .= new;
        is $got_a_code,          2, "using a coderef as a default value delays execution";
        is $was_in_closure,      0, "sub-coderef not yet executed";
        is try { $foo.code() }, 42, "sub-coderef execution works";
        is $was_in_closure,      1, "sub-coderef still not executed";
    }
}
