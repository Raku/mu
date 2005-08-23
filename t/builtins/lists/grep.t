#!/usr/bin/pugs

use v6;
use Test;

=kwid

built-in grep tests

=cut

plan 27;

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

# .grep shouldn't work on non-arrays
{
  dies_ok { 42.grep:{ $_ } },    "method form of grep should not work on numbers";
  dies_ok { "str".grep:{ $_ } }, "method form of grep should not work on strings";
  is ~(42,).grep:{ 1 }, "42",    "method form of grep should work on arrays";
}
