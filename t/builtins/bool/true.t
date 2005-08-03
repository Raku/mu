#!/usr/bin/pugs

use v6;
use Test;

plan 10;

ok(true 1,     "true 1 is true");
ok(true -1,    "true -1 is true");
ok(!(true 0),  "!true 0 is true");
ok(true sub{}, 'true sub{} is true');
ok(true "x",   'true "x" is true');

my $a = 1; ok(true $a,    'true $true_var is true');
my $b = 0; ok(!(true $b), 'true $false_var is not true');

ok( true(true 42), "true(true 42) is true");
ok(!true(true  0), "true(true  0) is false");

# no, bare 'true' in a when clause is acting as a function :)
# see t/statements/given.t  --mugwump
# Right, but there's the enum "true", too, see for example S12:
#     0 but true
#   is short for something like:
#     0 but bool::true
# :)  --iblech
# L<S04/"Switch statements" /"is exactly equivalent to">
eval_ok('true; 1', "bare 'true' is true", :todo<feature>);
