#!/usr/bin/pugs

use v6;
require Test;

=kwid

built-in map tests

=cut

plan 18;

my @list = (1 .. 5);


my @result1 = map { $_ * 2 } @list;
is(+@result1, 5, 'we got a list back');
is(@result1[0], 2, 'got the value we expected');
is(@result1[1], 4, 'got the value we expected');
is(@result1[2], 6, 'got the value we expected');
is(@result1[3], 8, 'got the value we expected');
is(@result1[4], 10, 'got the value we expected');
my @result2 = @list.map():{ $_ * 2 };

is(+@result2, 5, 'we got a list back');
is(@result2[0], 2, 'got the value we expected');
is(@result2[1], 4, 'got the value we expected');
is(@result2[2], 6, 'got the value we expected');
is(@result2[3], 8, 'got the value we expected');
is(@result2[4], 10, 'got the value we expected');

my @result3 = @list.map:{ $_ * 2 };
is(+@result3, 5, 'we got a list back');
is(@result3[0], 2, 'got the value we expected');
is(@result3[1], 4, 'got the value we expected');
is(@result3[2], 6, 'got the value we expected');
is(@result3[3], 8, 'got the value we expected');
is(@result3[4], 10, 'got the value we expected');

my @result4 = map { $_ * 2 }, @list;
is(+@result4, 5, 'we got a list back');
is(@result4[0], 2, 'got the value we expected');
is(@result4[1], 4, 'got the value we expected');
is(@result4[2], 6, 'got the value we expected');
is(@result4[3], 8, 'got the value we expected');
is(@result4[4], 10, 'got the value we expected');
