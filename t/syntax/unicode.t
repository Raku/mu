#!/usr/bin/pugs

use v6;
require Test;

# XXX - stops right here if GHC doesn't have unicode support.
if eval 'my $二 = 2; sub 恆等($x) {$x}; 恆等($二)' != 2 {
    plan 2;
    ok(eval 'my $foo; sub foo {}; 1', "ascii declaration");
    skip("GHC is not in unicode mode; try setting LANG to UTF-8 mode?");
    exit();
}

plan 18;

# english ;-)
ok(eval 'my $foo; sub foo {}; 1', "ascii declaration");
is(eval 'my $bar = 2; sub id ($x) { $x }; id($bar)', 2, "evaluation"); 

# umlauts
ok(eval 'my $übervar; sub fü {}; 1', "umlauts declaration");
is(eval 'my $schloß = 2; sub öok ($x) { $x }; öok($schloß)', 2, "evaluation");

# monty python
ok(eval 'my $møøse; sub bïte {};', "a møøse once bit my sister");
is(eval 'my $møøse = 2; sub såck ($x) { $x }; såck($møøse)', 2, "møøse bites kan be preti nasti");

# french
ok(eval 'my $un_variable_français; sub blâ {}; 1', "french declaration");
is(eval 'my $frénch = 2; sub bléch ($x) { $x }; bléch($frénch)', 2, "evaluation");

# Some Chinese Characters
ok(eval 'my $一; 1', "chinese declaration");
is(eval 'my $二 = 2; sub 恆等($x) {$x}; 恆等($二)', 2, "evaluation");

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
ok(eval 'my $פוו; sub לה {}; 1', "hebrew declaration");
is(eval 'my $באר = 2; sub זהות ($x) { $x }; זהות($באר)', 2, "evaluation");
