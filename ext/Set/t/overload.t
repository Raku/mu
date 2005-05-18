#!/usr/bin/pugs

use v6;
use Test;

plan 29;

use Set;

class Person {};

my $bob = Person.new;
my $bert = Person.new;

my $set = set(0, 1, 2, 3, $bob);
my $union = $set + set(4,5,6);
is($union.ref, ::Set, "set() - infix:<+>", :todo<bug>);

my $stringified = "$set";
ok($stringified ~~ rx:perl5/^set\([^<]*<obj:Person>[^<]*\)$/,
   "prefix:<~>", :todo<bug>);
diag("stringified to $stringified");

ok($union == set(0..6, $bob), "set() - infix:<==>");
ok(!($union != set(0..6, $bob)), "set() - !infix:<!=>");

ok($union != set(0..5, $bob), "set() - infix:<!=>", :todo<bug>);
ok(!($union == set(0..5, $bob)), "set() - !infix:<==>", :todo<bug>);

ok($union != set(0..6, $bob, $bert), "set() - infix:<!=>", :todo<bug>);
ok(!($union == set(0..6, $bob, $bert)), "set() - !infix:<==>", :todo<bug>);

my $other_set = set(2..3, 7, $bob, $bert);

my $intersection = $set * $other_set;
is($intersection, set(2..3, $bob), "intersection", :todo<bug>);

my $difference = $set - $other_set;
is($difference, set(0,1), "difference", :todo<bug>);

my $sym_difference = $set % $other_set;
is($sym_difference, set(0,1,7,$bert), "symmetric_difference", :todo<bug>);

is( ($set - $other_set) + ($other_set - $set), $set % $other_set,
    "long form of symmetric difference", :todo<bug>);

my ($homer, $marge, $bart, $lisa, $maggie) = (1..5).map:{ Person.new };

my $simpsons = set($homer, $marge, $bart, $lisa, $maggie);
my $parents = set($homer, $marge);
my $empty = set();

ok($parents < $simpsons, 'infix:"<"', :todo<bug>);
ok(!($simpsons < $parents), '!infix:"<"');
ok(!($parents < $parents), '!infix:"<" (equal sets)');

ok($parents <= $simpsons, 'infix:"<="');
ok(!($simpsons <= $parents), '!infix:"<="', :todo<bug>);
ok($parents <= $parents, 'infix:"<=" (equal sets)');

ok($empty < $simpsons, "infix:'<' (empty)", :todo<bug>);
ok($empty <= $simpsons, "infix:'<=' (empty)");

ok($simpsons > $parents, "infix:'>'", :todo<bug>);
ok(!($parents > $simpsons), "!infix:'>'");
ok(!($parents > $parents), "!infix:'>' (equal sets)");

ok($simpsons >= $parents, "infix:'>='");
ok(!($parents >= $simpsons), "!infix:'>='", :todo<bug>);
ok($parents >= $parents, "infix:'>=' (equal sets)");

ok($simpsons > $empty, "infix:'>' (empty)", :todo<bug>);
ok($parents >= $empty, "infix:'>=' (empty)");

eval_ok('set(1,2,3) ∋ 1', "infix:<∋>", :todo<bug>);
