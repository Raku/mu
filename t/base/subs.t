#!/usr/bin/pugs

use v6;
require Test;

plan 11;

sub foo_scalar {
    my $foo = 'foo';
    return $foo;
}
is(foo_scalar(), 'foo', 'got the right return value');

sub foo_array {
    my @foo = ('foo', 'bar', 'baz');
    return @foo;
}
my @foo_array_return = foo_array();
is(ref(@foo_array_return), 'Array', 'got an array back');
is(+@foo_array_return, 3, 'got the right number of return value');
is(@foo_array_return[0], 'foo', 'got the right return value');
is(@foo_array_return[1], 'bar', 'got the right return value');
is(@foo_array_return[2], 'baz', 'got the right return value');


# this test should be returning the list ref, but it is not

sub foo_array_ref {
    my $foo = ['foo', 'bar', 'baz'];
    return $foo;
}
my $foo_array_ref_return = foo_array();
todo_is(ref($foo_array_ref_return), 'List', 'got an List back');
todo_is(+$foo_array_ref_return, 3, 'got the right number of return value');
todo_is($foo_array_ref_return[0], 'foo', 'got the right return value');
todo_is($foo_array_ref_return[1], 'bar', 'got the right return value');
todo_is($foo_array_ref_return[2], 'baz', 'got the right return value');