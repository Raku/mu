#!/usr/bin/pugs

use v6;
require Test;

=kwid

Push tests

=cut

plan 27;

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

@push2.push(200);
is(+@push2, 2, 'we have 2 elements in the list');
is(@push2[1], 200, '@push2.push(200) works');

@push2.push(400);
is(+@push2, 3, 'we have 3 elements in the list');
is(@push2[2], 400, '@push2.push(400) works');

# try pushing more than one element

my @push3 = ();

push @push3, (1, 2, 3);
is(+@push3, 3, 'we have 3 elements in the list');
is(@push3[0], 1, 'got the expected element');
is(@push3[1], 2, 'got the expected element');
is(@push3[2], 3, 'got the expected element');

my @val2 = (4, 5);
push @push3, @val2;  
is(+@push3, 5, 'we have 5 elements in the list');
is(@push3[3], 4, 'got the expected element');
is(@push3[4], 5, 'got the expected element');

# now for the push() on an uninitialized list issue

my @push4;

push @push4, 42;
is(+@push4, 1, 'we have 1 element in the list');
is(@push4[0], 42, 'got the element expected');

push @push4, 2000;
is(+@push4, 2, 'we have 1 element in the list');
is(@push4[0], 42, 'got the element expected');
is(@push4[1], 2000, 'got the element expected');

