#!/usr/bin/pugs

use v6;
require Test;

=kwid

built-in grep tests

=cut

plan 24;

my @list = (1 .. 10);

my @result1 = grep { ($_ % 2) }, @list;
is(+@result1, 5, 'we got a list back');
is(@result1[0], 1, 'got the value we expected');
is(@result1[1], 3, 'got the value we expected');
is(@result1[2], 5, 'got the value we expected');
is(@result1[3], 7, 'got the value we expected');
is(@result1[4], 9, 'got the value we expected');

my @result2 = @list.grep():{ ($_ % 2) };
is(+@result2, 5, 'we got a list back');
is(@result2[0], 1, 'got the value we expected');
is(@result2[1], 3, 'got the value we expected');
is(@result2[2], 5, 'got the value we expected');
is(@result2[3], 7, 'got the value we expected');
is(@result2[4], 9, 'got the value we expected');

my @result3 = @list.grep:{ ($_ % 2) };
is(+@result3, 5, 'we got a list back');
is(@result3[0], 1, 'got the value we expected');
is(@result3[1], 3, 'got the value we expected');
is(@result3[2], 5, 'got the value we expected');
is(@result3[3], 7, 'got the value we expected');
is(@result3[4], 9, 'got the value we expected');

# FIXME parsefail
my @result4; # = eval 'grep { ($_ % 2) } @list';
is(+@result4, 5, 'we got a list back'); # unTODOme
is(@result4[0], 1, 'got the value we expected'); # unTODOme
is(@result4[1], 3, 'got the value we expected'); # unTODOme
is(@result4[2], 5, 'got the value we expected'); # unTODOme
is(@result4[3], 7, 'got the value we expected'); # unTODOme
is(@result4[4], 9, 'got the value we expected'); # unTODOme
