#!/usr/bin/pugs

use v6;
require Test;

=pod

built-in grep tests

=cut

plan 12;

my @list = (1 .. 10);

my @result1 = grep { ($_ % 2) } @list;
todo_is(+@result1, 5, 'we got a list back');
todo_is(@result1[0], 1, 'got the value we expected');
todo_is(@result1[1], 3, 'got the value we expected');
todo_is(@result1[2], 5, 'got the value we expected');
todo_is(@result1[3], 7, 'got the value we expected');
todo_is(@result1[4], 9, 'got the value we expected');

my @result2 = @list.grep:{ ($_ % 2) };
is(+@result2, 5, 'we got a list back');
is(@result2[0], 1, 'got the value we expected');
is(@result2[1], 3, 'got the value we expected');
is(@result2[2], 5, 'got the value we expected');
is(@result2[3], 7, 'got the value we expected');
is(@result2[4], 9, 'got the value we expected');