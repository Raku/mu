#!/usr/bin/pugs

use v6;
require Test;

=kwid

built-in grep tests

=cut

plan 24;

my @list = (1 .. 10);

{
    my @result = grep { ($_ % 2) }, @list;
    is(+@result, 5, 'we got a list back');
    is(@result[0], 1, 'got the value we expected');
    is(@result[1], 3, 'got the value we expected');
    is(@result[2], 5, 'got the value we expected');
    is(@result[3], 7, 'got the value we expected');
    is(@result[4], 9, 'got the value we expected');
}

{
    my @result = @list.grep():{ ($_ % 2) };
    is(+@result, 5, 'we got a list back');
    is(@result[0], 1, 'got the value we expected');
    is(@result[1], 3, 'got the value we expected');
    is(@result[2], 5, 'got the value we expected');
    is(@result[3], 7, 'got the value we expected');
    is(@result[4], 9, 'got the value we expected');
}

{
    my @result = @list.grep:{ ($_ % 2) };
    is(+@result, 5, 'we got a list back');
    is(@result[0], 1, 'got the value we expected');
    is(@result[1], 3, 'got the value we expected');
    is(@result[2], 5, 'got the value we expected');
    is(@result[3], 7, 'got the value we expected');
    is(@result[4], 9, 'got the value we expected');
}

{
    my @result = grep { ($_ % 2) } @list;
    is(+@result, 5, 'we got a list back'); 
    is(@result[0], 1, 'got the value we expected'); 
    is(@result[1], 3, 'got the value we expected'); 
    is(@result[2], 5, 'got the value we expected'); 
    is(@result[3], 7, 'got the value we expected'); 
    is(@result[4], 9, 'got the value we expected'); 
}