#!/usr/bin/pugs

use v6;
require Test;

plan 16;

# english ;-)
ok(eval 'my $foo; 1', "ascii declaration");
is(eval 'my $bar = 2; $bar', 2, "evaluation"); 

# umlauts
ok(eval 'my $übervar; 1', "umlauts declaration");
is(eval 'my $schloß = 2; $schloß', 2, "evaluation");

# monty python
ok(eval 'my $møøse; 1', "a møøse once bit my sister");
is(eval 'my $møøse = 2; $møøse', 2, "møøse bites kan be preti nasti");

# french
ok(eval 'my $un_variable_français; 1', "french declaration");
is(eval 'my $frénch = 2; $frénch', 2, "evaluation");

# Some Chinese Characters
ok(eval 'my $一; 1', "chinese declaration");
is(eval 'my $二 = 2; $二', 2, "evaluation");

# Tibeten Characters
ok(eval 'my $༡; 1', "tibeten declaration");
is(eval 'my $༢ = 2; $༢', 2, "evaluation");

# Japanese
ok(eval 'my $い; 1', "japanese declaration");
is(eval 'my $に = 2; $に', 2, "evaluation");

# arabic
ok(eval 'my $الصفحة ; 1', "arabic declaration");
is(eval 'my $الصفحة = 2; $الصفحة', 2, "evaluation");

# hebrew
ok(eval 'my $פוו  ;1', "hebrew declaration");
is(eval 'my $באר = 2; $באר', 2, "evaluation");
