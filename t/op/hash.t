#!/usr/bin/pugs

use v6;
require Test;

plan 11;

skip '%hash1 = (\'key\' => \'value\') is not yet supported by pugs';
#my %hash1 = ('key' => 'value');
#is %hash1{'key'}, 'value';
skip '%hash1{\'key2\'} = \'value2\' is not yet supported by pugs';
#%hash1{'key2'} = 'value2';
#todo_is %hash1{'key2'}, 'value2', 'lvalue assignment';

my %hash2;
eval '%hash2 = (:one, :key<value>, :three(3))';
todo_is %hash2{'one'}, 1, 'colonpair :one';
todo_is %hash2{'key'}, 'value', 'colonpair :key<value>';
todo_is %hash2{'three'}, 3, 'colonpair :three(3)';

my $value;
eval '$value = %hash1<key>';
todo_is $value, 'value', 'fetch using %hash<>';
my @slice1 = %hash2{"one", "three"};
todo_is @slice1[0], 1, 'hash slice - @slice1[0]';
todo_is @slice1[1], 3, 'hash slice - @slice1[1]';

my @slice2;
eval '@slice2 = %hash2<three one>';
ok @slice2[0] != 5123123, 'wtfthingy(sic) bad fetch';
todo_is @slice2[0], 3, '%hash<> slice';
todo_is @slice2[1], 1, '%hash<> slice';
