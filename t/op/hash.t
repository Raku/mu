#!/usr/bin/pugs

use v6;
require Test;

=pod

Hash tests

=cut

plan 18;

# basic lvalue assignment

my %hash1; 
%hash1{"one"} = 5; 
is(%hash1{"one"}, 5, 'lvalue hash assignment works (w/ double quoted keys)');

%hash1{'one'} = 4; 
is(%hash1{'one'}, 4, 'lvalue hash re-assignment works (w/ single quoted keys)');

eval '%hash1{two} = 2'; 
todo_is(%hash1{"two"}, 2, 'lvalue hash assignment works (w/ un-quoted keys)');

my %hash1; 
%hash1<three> = 3; 
todo_is(%hash1<tree>, 3, 'lvalue hash assignment works (w/ unquoted style <key>)');

# basic hash creation w/ comma seperated key/values

my %hash2 = ("one", 1);
is(%hash2{"one"}, 1, 'comma seperated key/value hash creation works');
is(%hash2<one>, 1, 'unquoted <key> fetching works');

my %hash3 = ("one", 1, "two", 2);
is(%hash3{"one"}, 1, 'comma seperated key/value hash creation works with more than one pair');
is(%hash3{"two"}, 2, 'comma seperated key/value hash creation works with more than one pair');

# basic hash creation w/ => seperated key/values (pairs?)

my %hash4;
# eval '%hash4 = ("key" => "value")';
todo_is(%hash4{"key"}, 'value', '(key => value) seperated key/value has creation works');

# hash slicing

my %hash5 = ("one", 1, "two", 2, "three", 3);

my @slice1 = %hash5{"one", "three"};
is(+@slice1, 2, 'got the right amount of values from the %hash{} slice');
is(@slice1[0], 1, '%hash{} slice successfull');
is(@slice1[1], 3, '%hash{} slice successfull');

my @slice2;
eval '@slice2 = %hash5<three one>';
is(+@slice2, 2, 'got the right amount of values from the %hash<> slice');
is(@slice2[0], 3, '%hash<> slice was successful');
is(@slice2[1], 1, '%hash<> slice was successful');

# misc stuff ...

my %hash6;
eval '%hash6 = (:one, :key<value>, :three(3))';
todo_is %hash6{'one'}, 1, 'colonpair :one';
todo_is %hash6{'key'}, 'value', 'colonpair :key<value>';
todo_is %hash6{'three'}, 3, 'colonpair :three(3)';

