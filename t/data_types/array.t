#!/usr/bin/pugs

use v6;
require Test;

=kwid

Arrays

=cut

plan 58;
force_todo 48 .. 52;

# array of strings

my @array1 = ("foo", "bar", "baz");
isa_ok(@array1, 'Array');

is(+@array1, 3, 'the array1 has 3 elements');
is(@array1[0], 'foo', 'got the right value at array1 index 0');
is(@array1[1], 'bar', 'got the right value at array1 index 1');
is(@array1[2], 'baz', 'got the right value at array1 index 2');

is(@array1.[0], 'foo', 'got the right value at array1 index 0 using the . notation');

# array with strings, numbers and undef

my @array2 = ("test", 1, undef);
isa_ok(@array2, 'Array');

is(+@array2, 3, 'the array2 has 3 elements');
is(@array2[0], 'test', 'got the right value at array2 index 0');
is(@array2[1], 1,      'got the right value at array2 index 1');
is(@array2[2], undef,  'got the right value at array2 index 2');

# combine 2 arrays

my @array3 = (@array1, @array2);
isa_ok(@array3, 'Array');

is(+@array3, 6, 'the array3 has 6 elements'); # unTODOme
is(@array3[0], 'foo', 'got the right value at array3 index 0'); # unTODOme
is(@array3[1], 'bar', 'got the right value at array3 index 1'); # unTODOme
is(@array3[2], 'baz', 'got the right value at array3 index 2'); # unTODOme
is(@array3[3], 'test', 'got the right value at array3 index 3'); # unTODOme
is(@array3[4], 1,      'got the right value at array3 index 4'); # unTODOme
is(@array3[5], undef,  'got the right value at array3 index 5');

# array slice

my @array4 = @array2[2, 1, 0];
isa_ok(@array4, 'Array');

is(+@array4, 3, 'the array4 has 3 elements');
is(@array4[0], undef,  'got the right value at array4 index 0');
is(@array4[1], 1,      'got the right value at array4 index 1');
is(@array4[2], 'test', 'got the right value at array4 index 2');

# create new array with 2 array slices

my @array5 = ( @array2[2, 1, 0], @array1[2, 1, 0] );
isa_ok(@array5, 'Array');

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
isa_ok(@array6, 'Array');

is(+@array6, 3, 'the array6 has 3 elements'); # unTODOme
is(@array6[0], 'baz', 'got the right value at array6 index 0'); # unTODOme
is(@array6[1], 'foo', 'got the right value at array6 index 1'); # unTODOme
is(@array6[2], 'bar', 'got the right value at array6 index 2'); # unTODOme

# create an array slice with an array constructed with ()

my @array7 = @array1[(2, 1, 0)];
isa_ok(@array7, 'Array');

is(+@array7, 3, 'the array7 has 3 elements');
is(@array7[0], 'baz', 'got the right value at array7 index 0');
is(@array7[1], 'bar', 'got the right value at array7 index 1');
is(@array7[2], 'foo', 'got the right value at array7 index 2');

# odd slices

my $result1 = (1, 2, 3, 4)[1];
is($result1, 2, 'got the right value from the slice');

my $result2 = [1, 2, 3, 4][2];
is($result2, 3, 'got the right value from the slice');

# swap two elements test moved to t/op/assign.t

# empty arrays

my @array9;
isa_ok(@array9, 'Array');
is(+@array9, 0, "new arrays are empty");

my @array10 = (1, 2, 3,);
is(+@array10, 3, "trailing commas make correct list"); # unTODOme

# declare a multidimension array
todo_eval_ok('@array11[0...3; 0...1]', "multidimension array");
todo_eval_ok('@array11[2,0] = 12', "push the value to a multidimension array");
todo_eval_ok('@array12 is shape(2,4)', "another way to declare a multidimension array");

# declare the array with data type
todo_eval_ok('my Int @array', "declare a array for integer only");
todo_eval_ok('@array[0] = 23', "declare the array value");

# negative index
my @array11 = ('a', 'b', 'c', 'e'); 
is @array11[-1],'e', "negative index [-1]";

# negative index range
is ~@array11[-4 .. -2], 'a b c', "negative index [-4 .. -2]";

# negative index as lvalue
@array11[-1]   = 'd';
is @array11[-1], 'd', "assigns to the correct negative slice index"; 
is ~@array11,'a b c d', "assignment to neg index correctly alters the array";

my @array12 = ('a', 'b', 'c', 'd'); 
# negative index range as lvalue
@array12[-4 .. -1]   = ('d', 'c', 'b', 'a'); #('a'..'d').reverse
is ~@array12, 'd c b a', "negative range as lvalue"; 

#hat trick
my @array13 = ('a', 'b', 'c', 'd');
my @b = 0..3;
((@b[-3,-2,-1,-4] = @array13)= @array13[-1,-2,-3,-4]);

is ~@b, 
	'a d c b', 
	"hat trick:
	assign to a negatively indexed slice array from array  
	lvalue in assignment is then lvalue to negatively indexed slice as rvalue"; 
#
