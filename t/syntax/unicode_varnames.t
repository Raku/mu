#!/usr/bin/pugs

use v6;
require Test;

plan 16;

# english ;-)
ok(eval 'my $foo; 1', "ascii declaration");
is(eval 'my $bar = 2; $bar', 2, "evaluation"); 

# umlauts
todo_ok(eval 'my $übervar', "umlauts declaration");
todo_is(eval 'my $schloß = 2; $schloß', 2, "evaluation");

# monty python
todo_ok(eval 'my $møøse', "a møøse once bit my sister");
todo_is(eval 'my $møøse = 2', 2, "møøse bites kan be preti nasti");

# french
todo_ok(eval 'my $un_variable_français', "french declaration");
todo_is(eval 'my $frénch = 2; $frénch', 2, "evaluation");

# Some Chinese Characters
todo_ok(eval 'my $一; 1', "chinese declaration");
todo_is(eval 'my $二 = 2; $二', 2, "evaluation");

# Tibeten Characters
todo_ok(eval 'my $༡; 1', "tibeten declaration");
todo_is(eval 'my $༢ = 2; $༢', 2, "evaluation");

# arabic
todo_ok(eval 'my $ﭧﭟﭑ', "arabic declaration");
todo_is(eval 'my $ﭧﭟﭑ = 2; $ﭧﭟﭑ', "evaluation");

# hebrew
todo_ok(eval 'my $פוו', "hebrew declaration");
todo_is(eval 'my $באר = 2; $באר', 2, "evaluation");