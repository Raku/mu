#!/usr/bin/pugs

use v6;
use Test;

plan 28;

force_todo 11, 12;

use Set::Hash;

class Person {};

my $bob = Person.new;
my $bert = Person.new;

my $set = set(0, 1, 2, 3, $bob);
my $union = $set.union(set(4,5,6));
is($union.ref, ::Set::Hash, "set() - union");

my $stringified = $set.stringify;
ok($stringified ~~ rx:perl5/^set\([^<]*<obj:Person>[^<]*\)$/,
   "stringify");
diag("stringified to $stringified");

ok($union.equal(set(0..6, $bob)), "set() - equal");
ok(!$union.not_equal(set(0..6, $bob)), "set() - !not_equal");

ok($union.not_equal(set(0..5, $bob)), "set() - not_equal");
ok(!$union.equal(set(0..5, $bob)), "set() - !equal");

ok($union.not_equal(set(0..6, $bob, $bert)), "set() - not_equal");
ok(!$union.equal(set(0..6, $bob, $bert)), "set() - !equal");

my $other_set = set(2..3, 7, $bob, $bert);

my $intersection = $set.intersection($other_set);
ok($intersection.equal(set(2..3, $bob)), "intersection");

my $difference = $set.difference($other_set);
ok($difference.equal(set(0,1)), "difference");

my $sym_difference = try { $set.symmetric_difference($other_set) };
ok(try { $sym_difference.equal(set(0,1,7,$bert)) }, "symmetric_difference");

ok(try { $set.difference($other_set).union($other_set.difference($set))
	.equal($sym_difference) }, "long form of symmetric difference");

my ($homer, $marge, $bart, $lisa, $maggie) = (1..5).map:{ Person.new };

my $simpsons = set($homer, $marge, $bart, $lisa, $maggie);
my $parents = set($homer, $marge);
my $empty = set();

ok($parents.proper_subset($simpsons), "proper_subset");
ok(!$simpsons.proper_subset($parents), "!proper_subset");
ok(!$parents.proper_subset($parents), "!proper_subset (equal sets)");

ok($parents.subset($simpsons), "subset");
ok(!$simpsons.subset($parents), "!subset");
ok($parents.subset($parents), "subset (equal sets)");

ok($empty.proper_subset($simpsons), "proper_subset (empty)");
ok($empty.subset($simpsons), "subset (empty)");

ok($simpsons.proper_superset($parents), "proper_superset");
ok(!$parents.proper_superset($simpsons), "!proper_superset");
ok(!$parents.proper_superset($parents), "!proper_superset (equal sets)");

ok($simpsons.superset($parents), "superset");
ok(!$parents.superset($simpsons), "!superset");
ok($parents.superset($parents), "superset (equal sets)");

ok($simpsons.proper_superset($empty), "proper_superset (empty)");
ok($parents.superset($empty), "superset (empty)");
