#!/usr/bin/pugs

use v6;
require Test;

=pod

Arrays

=cut

plan 39;

# array of strings

my @array1 = ("foo", "bar", "baz");

is(+@array1, 3, 'the array1 has 3 elements');
is(@array1[0], 'foo', 'got the right value at array1 index 0');
is(@array1[1], 'bar', 'got the right value at array1 index 1');
is(@array1[2], 'baz', 'got the right value at array1 index 2');

is(@array1.[0], 'foo', 'got the right value at array1 index 0 using the . notation');

# array with strings, numbers and undef

my @array2 = ("test", 1, undef);

is(+@array2, 3, 'the array2 has 3 elements');
is(@array2[0], 'test', 'got the right value at array2 index 0');
is(@array2[1], 1,      'got the right value at array2 index 1');
is(@array2[2], undef,  'got the right value at array2 index 2');

# combine 2 arrays

my @array3 = (@array1, @array2);

is(+@array3, 6, 'the array3 has 6 elements');
is(@array3[0], 'foo', 'got the right value at array3 index 0');
is(@array3[1], 'bar', 'got the right value at array3 index 1');
is(@array3[2], 'baz', 'got the right value at array3 index 2');
is(@array3[3], 'test', 'got the right value at array3 index 3');
is(@array3[4], 1,      'got the right value at array3 index 4');
is(@array3[5], undef,  'got the right value at array3 index 5');

# array slice

my @array4 = @array2[2, 1, 0];

is(+@array4, 3, 'the array4 has 3 elements');
is(@array4[0], undef,  'got the right value at array4 index 0');
is(@array4[1], 1,      'got the right value at array4 index 1');
is(@array4[2], 'test', 'got the right value at array4 index 2');

# create new array with 2 array slices

my @array5 = [ @array2[2, 1, 0], @array1[2, 1, 0] ];

is(+@array5, 6, 'the array5 has 6 elements');
is(@array5[0], undef,  'got the right value at array5 index 0');
is(@array5[1], 1,      'got the right value at array5 index 1');
is(@array5[2], 'test', 'got the right value at array5 index 2');
is(@array5[3], 'baz',  'got the right value at array5 index 3');
is(@array5[4], 'bar',  'got the right value at array5 index 4');
is(@array5[5], 'foo',  'got the right value at array5 index 5');

# create an array slice with an array (in a variable)

my @slice = (2, 0, 1);
my @array6 = @array1[@slice];

is(+@array6, 3, 'the array6 has 3 elements');
is(@array6[0], 'baz', 'got the right value at array6 index 0');
is(@array6[1], 'foo', 'got the right value at array6 index 1');
is(@array6[2], 'bar', 'got the right value at array6 index 2');

# create an array slice with an array constructed with []

my @array7 = @array1[[2, 1, 0]];

is(+@array7, 3, 'the array7 has 3 elements');
is(@array7[0], 'baz', 'got the right value at array7 index 0');
is(@array7[1], 'bar', 'got the right value at array7 index 1');
is(@array7[2], 'foo', 'got the right value at array7 index 2');

# odd slices

my $result1 = (1, 2, 3, 4)[1];
is($result1, 2, 'got the right value from the slice');

my $result2 = [1, 2, 3, 4][2];
is($result2, 3, 'got the right value from the slice');


# swap two elements
my @a = (1,2,3,4,5);
@a[0,1] = @a[1,0];
is(@a[0], 2, "slice assignment awaiting p6cabal clarification");
is(@a[1], 1, "slice assignment awaiting p6cabal clarification");
