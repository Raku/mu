#!/usr/bin/pugs

use v6;
require Test;

=kwid

Hash tests

=cut

plan 49;

# basic lvalue assignment

my $hash1; 
isa_ok($hash1, 'Any');

$hash1{"1st"} = 5; 
isa_ok($hash1, 'Hash');

is($hash1{"1st"}, 5, 'lvalue hash assignment works (w/ double quoted keys)');

$hash1{'1st'} = 4; 
is($hash1{'1st'}, 4, 'lvalue hash re-assignment works (w/ single quoted keys)');

## unquoted hash keys are now illegal AFAIK
# eval '$hash1{2nd} = 2'; 
# todo_is($hash1{"2nd"}, 2, 'lvalue hash assignment works (w/ un-quoted keys)');

$hash1<3rd> = 3; 
is($hash1<3rd>, 3, 'lvalue hash assignment works (w/ unquoted style <key>)');

# basic hash creation w/ comma seperated key/values

my $hash2 = ("1st", 1);
isa_ok($hash2, 'List');
is($hash2{"1st"}, 1, 'comma seperated key/value hash creation works');
is($hash2<1st>, 1, 'unquoted <key> fetching works');

my $hash3 = ("1st", 1, "2nd", 2);
isa_ok($hash3, 'List');
is($hash3{"1st"}, 1, 'comma seperated key/value hash creation works with more than 1st pair');
is($hash3{"2nd"}, 2, 'comma seperated key/value hash creation works with more than 1st pair');

# hash slicing

my $hash5 = ("1st", 1, "2nd", 2, "3rd", 3);
isa_ok($hash5, 'List');

my @slice1 = $hash5{"1st", "3rd"};
is(+@slice1, 2, 'got the right amount of values from the %hash{} slice');
is(@slice1[0], 1, '%hash{} slice successfull');
is(@slice1[1], 3, '%hash{} slice successfull');

my @slice2;
eval '@slice2 = $hash5<3rd 1st>';
is(+@slice2, 2, 'got the right amount of values from the %hash<> slice');
is(@slice2[0], 3, '%hash<> slice was successful');
is(@slice2[1], 1, '%hash<> slice was successful');

# slice assignment

eval '$hash5{"1st", "3rd"} = (5, 10)';
is($hash5<1st>, 5, 'value was changed successfully with slice assignment');
is($hash5<3rd>, 10, 'value was changed successfully with slice assignment');

eval '$hash5<1st 3rd> = [3, 1]';
is($hash5<1st>, 3, 'value was changed successfully with slice assignment');
is($hash5<3rd>, 1, 'value was changed successfully with slice assignment');

# keys 

my $hash6 = ("1st", 1, "2nd", 2, "3rd", 3);
isa_ok($hash6, 'List');

my @keys1 = sort keys $hash6;
is(+@keys1, 3, 'got the right number of keys');
is(@keys1[0], '1st', 'got the right key');
is(@keys1[1], '2nd', 'got the right key');
is(@keys1[2], '3rd', 'got the right key');

my @keys2 = sort $hash6.keys;
is(+@keys2, 3, 'got the right number of keys');
is(@keys2[0], '1st', 'got the right key');
is(@keys2[1], '2nd', 'got the right key');
is(@keys2[2], '3rd', 'got the right key');

# values

my $hash7 = ("1st", 1, "2nd", 2, "3rd", 3);
isa_ok($hash7, 'List');

my @values1 = values $hash7;
is(+@values1, 3, 'got the right number of values');
is(@values1[0], 1, 'got the right values');
is(@values1[1], 2, 'got the right values');
is(@values1[2], 3, 'got the right values');

my @values1 = $hash7.values;
is(+@values1, 3, 'got the right number of values');
is(@values1[0], 1, 'got the right values');
is(@values1[1], 2, 'got the right values');
is(@values1[2], 3, 'got the right values');

# hashref assignment using {}
# L<S06/"Anonymous hashes vs blocks" /"So you may use sub or hash or pair to disambiguate:">
my $hash8_a = { a => 1, b => 2 };             todo_isa_ok $hash8_a, "Hash";
my $hash8_b = { a => 1, "b", 2 };             todo_isa_ok $hash8_b, "Hash";
my $hash8_c = eval 'hash(a => 1, "b", 2)';    todo_isa_ok $hash8_c, "Hash";
my $hash8_d = eval 'hash a => 1, "b", 2';     todo_isa_ok $hash8_d, "Hash";
my $hash8_e = eval '{ pair "a", 1, "b", 2 }'; todo_isa_ok $hash8_e, "Hash";

# recursive hash
my %hash9 = (val => 42);
%hash9{"ref"} = \%hash9;
isa_ok %hash9,        "Hash";
isa_ok %hash9{"ref"}, "Hash";
is %hash9{"ref"}{"val"},          42, "access to recursive hash (1)";
is %hash9{"ref"}.{"ref"}.{"val"}, 42, "access to recursive hash (2)";
