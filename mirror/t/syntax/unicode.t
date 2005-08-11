#!/usr/bin/pugs

use v6;
use Test;

plan 18;

# english ;-)
ok(try {my $foo; sub foo {}; 1}, "ascii declaration");
is(try {my $bar = 2; sub id ($x) { $x }; id($bar)}, 2, "evaluation"); 

# umlauts
ok(try {my $übervar; sub fü {}; 1}, "umlauts declaration");
is(try {my $schloß = 2; sub öok ($x) { $x }; öok($schloß)}, 2, "evaluation");

# monty python
ok(try {my $møøse; sub bïte {}; 1}, "a møøse once bit my sister");
is(try {my $møøse = 2; sub såck ($x) { $x }; såck($møøse)}, 2, "møøse bites kan be preti nasti");

# french
ok(try {my $un_variable_français; sub blâ {}; 1}, "french declaration");
is(try {my $frénch = 2; sub bléch ($x) { $x }; bléch($frénch)}, 2, "evaluation");

# Some Chinese Characters
ok(try {my $一; 1}, "chinese declaration");
is(try {my $二 = 2; sub 恆等($x) {$x}; 恆等($二)}, 2, "evaluation");

# Tibetan Characters
ok(try {my $༡; 1}, "tibetan declaration");
is(try {my $༢ = 2; $༢}, 2, "evaluation");

# Japanese
ok(try {my $い; 1}, "japanese declaration");
is(try {my $に = 2; $に}, 2, "evaluation");

# arabic
ok(try {my $الصفحة ; 1}, "arabic declaration");
is(try {my $الصفحة = 2; $الصفحة}, 2, "evaluation");

# hebrew
ok(try {my $פוו; sub לה {}; 1}, "hebrew declaration");
is(try {my $באר = 2; sub זהות ($x) { $x }; זהות($באר)}, 2, "evaluation");
