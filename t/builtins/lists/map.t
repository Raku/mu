#!/usr/bin/pugs

use v6;
use Test;

=kwid

built-in map tests

=cut

plan 41;

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

# Testing map that returns an array
{
    my @result = map { ($_, $_ * 2) }, @list;
    is(+@result, 10, 'we got a list back');
    is(@result[0], 1, 'got the value we expected');
    is(@result[1], 2, 'got the value we expected');
    is(@result[2], 2, 'got the value we expected');
    is(@result[3], 4, 'got the value we expected');
    is(@result[4], 3, 'got the value we expected');
    is(@result[5], 6, 'got the value we expected');
    is(@result[6], 4, 'got the value we expected');
    is(@result[7], 8, 'got the value we expected');
    is(@result[8], 5, 'got the value we expected');
    is(@result[9], 10, 'got the value we expected');
}

# Testing multiple statements in the closure
{
    my @result = map {
         my $fullpath = "fish/$_";
         $fullpath;
    }, @list;
    is(+@result, 5, 'we got a list back');
    is(@result[0], "fish/1", 'got the value we expected');
    is(@result[1], "fish/2", 'got the value we expected', :todo<bug>);
    is(@result[2], "fish/3", 'got the value we expected', :todo<bug>);
    is(@result[3], "fish/4", 'got the value we expected', :todo<bug>);
    is(@result[4], "fish/5", 'got the value we expected', :todo<bug>);
}