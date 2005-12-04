#!/usr/bin/pugs

use v6;
use Test;

=pod

Tests that die() preserves the data type of its argument, 
and does not cast its argument as a Str.

=cut

plan 10;

try {
    my Bool $foo = bool::true;
    is( $foo.ref, 'Bool', 'arg to be given as die() arg contains a Bool value' );
    die $foo;
};
is( $!.ref, 'Bool', 'following eval { die() } with Bool arg, $! contains a Bool value' );

try {
    my Int $foo = 42;
    is( $foo.ref, 'Int', 'arg to be given as die() arg contains a Int value' );
    die $foo;
};
is( $!.ref, 'Int', 'following eval { die() } with Int arg, $! contains a Int value' );

try {
    my Str $foo = 'hello world';
    is( $foo.ref, 'Str', 'arg to be given as die() arg contains a Str value' );
    die $foo;
};
is( $!.ref, 'Str', 'following eval { die() } with Str arg, $! contains a Str value' );

try {
    my Pair $foo = 'question' => 'answer';
    is( $foo.ref, 'Pair', 'arg to be given as die() arg contains a Pair value' );
    die $foo;
};
is( $!.ref, 'Pair', 'following eval { die() } with Pair arg, $! contains a Pair value' );

try {
    my Object $foo .= new();
    is( $foo.ref, 'Object', 'arg to be given as die() arg contains a Object value' );
    die $foo;
};
is( $!.ref, 'Object', 'following eval { die() } with Object arg, $! contains a Object value' );
