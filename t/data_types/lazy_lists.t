#!/usr/bin/pugs

use v6;
use Test;

die "$?PUGS_BACKEND does not support lazy lists yet"
    if ! $?PUGS_BACKEND eq "BACKEND_PERL5";

# constructors

is( (1..Inf).perl, 
    "(1, 2, 3 ... Inf)", 
    "simple infinite list" );

is( (1...).perl, 
    "(1, 2, 3 ... Inf)", 
    "simple infinite list" );

is( (-Inf..Inf).perl, 
    "-Inf ... Inf)", 
    "double infinite list" );

is( (-Inf..0).perl, 
    "-Inf ... -2, -1, 0)", 
    "negative infinite list" );

is( ('aaaa'..'zzzz').perl, 
    "'aaaa', 'aaab', 'aaac' ... 'zzzx', 'zzzy', 'zzzz')", 
    "string lazy list" );

is( ('x' xx 1000000).perl,
    "('x', 'x', 'x' ... 'x', 'x', 'x')",
    "xx operator" );

# splices

{
    my @a = (1..Inf);
    is( @a.splice( 2, 3 ), 
        "(3, 4, 5)",
        "splice" );
    is( @a, 
        "(1, 2, 6 ... Inf)",
        "spliced" );
}

{
    my @a = (1..Inf);
    is( @a.splice( 2, Inf, 99, 100 ), 
        "(3, 4, 5 ... Inf)",
        "splice" );
    is( @a, 
        "(1, 2, 99, 100)",
        "spliced" );
}

# basic list operations

is( (1..Inf).reverse.perl, 
    "(Inf ... 3, 2, 1)", 
    "reverse" );

is( (1..Inf).map:{ $_/2 }.perl, 
    "(0.5, 1, 1.5 ... Inf)", 
    "map" );

# slices

is( (1..Inf)[2..5].perl,
    "(3, 4, 5, 6)",
    "simple slice" );

{
    my @a = (1..Inf);
    is( @a[2..5].perl,
        "(3, 4, 5, 6)",
        "simple slice" );
}

is( (1..Inf)[2..Inf].perl,
    "(3, 4, 5 ... Inf)",
    "lazy slice" );

=for later
is( (1..Inf)[2..100000].perl,
    "(3, 4, 5 ... 100001, 100002, 100003)",
    "lazy slice" );
=cut
