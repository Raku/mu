#!/usr/bin/pugs

use v6;
require Test;

plan 15;

## comparison operators <, <=, <=>, >=, >

# less than

ok(1 < 2, '1 is less than 2');
ok(!(2 < 1), '2 is ~not~ less than 1');

# greater than

ok(2 > 1, '2 is greater than 1');
ok(!(1 > 2), '1 is ~not~ greater than 2');

# less than or equal to

ok(1 <= 2, '1 is less than or equal to 2');
ok(1 <= 1, '1 is less than or equal to 2');
ok(!(1 <= 0), '1 is ~not~ less than or equal to 0');

# greater than or eqaul to

ok(2 >= 1, '2 is greater than or equal to 1');
ok(2 >= 2, '2 is greater than or equal to 2');
ok(!(2 >= 3), '2 is ~not~ greater than or equal to 3');

# spaceship

is(1 <=> 1, 0,  '1 is equal to 1');
is(1 <=> 2, -1, '1 is less than 2');
is(2 <=> 1, 1,  '2 is greater than 2');

## Multiway comparisons (RFC 025)

# this works ...
todo_is(5 > 1 < 10, 5 > 1 && 1 < 10, 'multi-way comp 5 > 1 < 10 works');

# however this doesn't which makes 
# me think these are not implemented
is(5 < 1 < 10, 5 < 1 && 1 < 10, 'multi-way comp 5 < 1 < 10 works');