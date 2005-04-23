#!/usr/bin/pugs

use v6;
require Test;

plan 8;
force_todo 8;

ok(true 1,     "true 1 is true");
ok(true -1,    "true -1 is true");
ok(!(true 0),  "!true 0 is true");
ok(true sub{}, 'true sub{} is true');
ok(true "x",   'true "x" is true');

my $a = 1; ok(true $a,    'true $true_var is true');
my $b = 0; ok(!(true $b), 'true $false_var is not true');

# L<S04/"Switch statements" /"is exactly equivalent to">
eval_ok('true', "bare 'true' is true"); 
