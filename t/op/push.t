#!/usr/bin/pugs

use v6;
require Test;

=kwid

Push tests

=cut

# NOTE:
# When push(@l) is performed on an uninitialized list, will 
# result in a 2 element list. 
# This code:
#     pugs -e 'my @l; push @l, 10; say join ", ", @l; say +@l;'
# Produces the following output:
#     , 10
#     2
# Note the leading comma. The issue exists no matter which way
# push is written. Both these examples produce the same output
# as above. 
#     pugs -e 'my @l; push @l, (10); say join ", ", @l;'
#     pugs -e 'my @l; push(@l, 10); say join ", ", @l;' 
#     
# However, the following code:
#     pugs -e 'my @l = (); push(@l, 10); say join ", ", @l; say +@l;' 
# Produces the correct output:
#     10
#     1
#
# See the end of this file for tests which illustrate this issue.

plan 28;

# basic push tests

my @push1 = ();

is(+@push1, 0, 'we have an empty list');

push(@push1, 1);
is(+@push1, 1, 'we have 1 element in the list');
is(@push1[0], 1, 'we found the right element');

push(@push1, 2);
is(+@push1, 2, 'we have 2 elements in the list');
is(@push1[1], 2, 'we found the right element');

push(@push1, 3);
is(+@push1, 3, 'we have 3 element in the list');
is(@push1[2], 3, 'we found the right element');

push(@push1, 4);
is(+@push1, 4, 'we have 4 element in the list');
is(@push1[3], 4, 'we found the right element');

# try other variations on calling push()

my @push2 = ();

my $val = 100;

push @push2, $val;
is(+@push2, 1, 'we have 1 element in the list');
is(@push2[0], $val, 'push @list, $val worked');

eval '@push2.push(200)';
todo_is(+@push2, 2, 'we have 2 elements in the list');
todo_is(@push2[1], 200, '@push2.push(200) works');

eval '@push2.push 400';
todo_is(+@push2, 3, 'we have 3 elements in the list');
todo_is(@push2[2], 400, '@push2.push(400) works');

# try pushing more than one element

my @push3 = ();

push @push3, (1, 2, 3);
is(+@push3, 3, 'we have 3 elements in the list');
is(@push3[0], 1, 'got the expected element');
is(@push3[1], 2, 'got the expected element');
is(@push3[2], 3, 'got the expected element');

my @val2 = (4, 5);
push @push3, @val2;  # the lists should not flatten here
todo_is(+@push3, 4, 'we have 4 elements in the list');
todo_is(+@push3[3], 2, 'we have 2 elements in the sub-list');
is(@push3[3][0], 4, 'got the expected element');
todo_is(@push3[3][1], 5, 'got the expected element');

# now for the push() on an uninitialized list issue

my @push4;

push @push4, 42;
todo_is(+@push4, 1, 'we have 1 element in the list');
todo_is(@push4[0], 42, 'got the element expected');

push @push4, 2000;
todo_is(+@push4, 2, 'we have 1 element in the list');
todo_is(@push4[0], 42, 'got the element expected');
todo_is(@push4[1], 2000, 'got the element expected');

