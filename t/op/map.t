#!/usr/bin/pugs

use v6;
require Test;

=pod

built-in map tests

=cut

plan 6;

my @list = (1 .. 5);

my @result = map { $_ * 2 } @list;
todo_is(+@result, 5, 'we got a list back');
todo_is(@result[0], 2, 'got the value we expected');
todo_is(@result[1], 4, 'got the value we expected');
todo_is(@result[2], 6, 'got the value we expected');
todo_is(@result[3], 8, 'got the value we expected');
todo_is(@result[4], 10, 'got the value we expected');