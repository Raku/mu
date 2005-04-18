#!/usr/bin/pugs

use v6;
require Test;

=kwid

built-in map tests

=cut

plan 24;

my @list = (1 .. 5);

{
    my @result = map { $_ * 2 } @list;
    is(+@result, 5, 'we got a list back'); 
    is(@result[0], 2, 'got the value we expected'); 
    is(@result[1], 4, 'got the value we expected'); 
    is(@result[2], 6, 'got the value we expected'); 
    is(@result[3], 8, 'got the value we expected'); 
    is(@result[4], 10, 'got the value we expected'); 
}

{
    my @result = @list.map():{ $_ * 2 };
    is(+@result, 5, 'we got a list back');
    is(@result[0], 2, 'got the value we expected');
    is(@result[1], 4, 'got the value we expected');
    is(@result[2], 6, 'got the value we expected');
    is(@result[3], 8, 'got the value we expected');
    is(@result[4], 10, 'got the value we expected');
}

{
    my @result = @list.map:{ $_ * 2 };
    is(+@result, 5, 'we got a list back');
    is(@result[0], 2, 'got the value we expected');
    is(@result[1], 4, 'got the value we expected');
    is(@result[2], 6, 'got the value we expected');
    is(@result[3], 8, 'got the value we expected');
    is(@result[4], 10, 'got the value we expected');
}

{
    my @result = map { $_ * 2 }, @list;
    is(+@result, 5, 'we got a list back');
    is(@result[0], 2, 'got the value we expected');
    is(@result[1], 4, 'got the value we expected');
    is(@result[2], 6, 'got the value we expected');
    is(@result[3], 8, 'got the value we expected');
    is(@result[4], 10, 'got the value we expected');
}
