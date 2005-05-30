#!/usr/bin/pugs

use v6;
use Test;

plan 2;

class Parent {}

my $new_count = 0;

class Child is Parent
{
    method new ( $Class: )
    {
        return if $new_count++ > 1;
        return $Class.SUPER::new();
    }
}

my $child = Child.new;

is( $child.ref, 'Child', 'new() should be SUPER-callable' );
is( $new_count, 1,
    'SUPER should not call itself in the absence of a parent method' );
