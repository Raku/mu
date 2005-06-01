#!/usr/bin/pugs

# It's the Unicode overload!

use v6;
use Test;

plan 42;
force_todo 1, 2, 7, 9, 14..20, 23..24, 30..34;

use Set;

class Person {};

my $bob = Person.new;
my $bert = Person.new;

my $set = set(0, 1, 2, 3, $bob);
my $union = eval '$set ∪ set(4,5,6)';
is($union.ref, ::Set, "set() - infix:<∪>");

ok($union == set(0..6, $bob), "set() - infix:<==>");
ok(!(eval '$union ≠ set(0..6, $bob)'), "set() - !infix:<≠>");

my $other_set = set(2..3, 7, $bob, $bert);

my $intersection = eval '$set ∩ $other_set';
is($intersection, set(2..3, $bob), "intersection");

# Yes, this operator is PURE EVIL
my $difference = eval '$set ∖ $other_set';
is($difference, set(0,1), "difference");

# there doesn't seem to be a unicode operator for symmetric
# difference.
my $sym_difference = $set % $other_set;
is($sym_difference, set(0,1,7,$bert), "symmetric_difference");

my ($homer, $marge, $bart, $lisa, $maggie) = (1..5).map:{ Person.new };

my $simpsons = set($homer, $marge, $bart, $lisa, $maggie);
my $parents = set($homer, $marge);

ok(eval('$parents ⊂ $simpsons'), 'infix:"⊂"');
ok(!(eval('$simpsons ⊂ $parents')), '!infix:"⊂"');
ok(!(eval('$parents ⊄ $simpsons')), '!infix:"⊄"');
ok(eval('$simpsons ⊄ $parents'), 'infix:"⊄"');

# open question - should ⊂ mean ⊊ or ⊆ ?  Personally, I think ⊊ is
# superfluous.
ok(!(eval('$parents ⊂ $parents')), '!infix:"⊂" (equal sets)');
ok(eval('$parents ⊄ $parents'), 'infix:"⊄" (equal sets)');
ok(!(eval('$parents ⊊ $parents')), '!infix:"⊊" (equal sets)');

ok(eval('$parents ⊆ $simpsons'), 'infix:"⊆"');
ok(!(eval('$parents ⊈ $simpsons')), '!infix:"⊈"');
ok(!(eval('$simpsons ⊆ $parents')), '!infix:"⊆"');
ok(eval('$simpsons ⊈ $parents'), 'infix:"⊈"');
ok(eval('$parents ⊆ $parents'), 'infix:"⊆" (equal sets)');

ok(eval('∅ ⊂ $simpsons'), "infix:'⊂' (empty)");
ok(eval('∅ ⊆ $simpsons'), "infix:'⊆' (empty)");
ok(!(eval('∅ ⊄ $simpsons')), "!infix:'⊄' (empty)");
ok(!(eval('∅ ⊈ $simpsons')), "!infix:'⊈' (empty)");

ok(eval('$simpsons ⊃ $parents'), "infix:'⊃'");
ok(!(eval('$simpsons ⊅ $parents')), "!infix:'⊅'");
ok(!(eval('$parents ⊃ $simpsons')), "!infix:'⊃'");
ok(eval('$parents ⊅ $simpsons'), "infix:'⊅'");

ok(!(eval('$parents ⊃ $parents')), "!infix:'⊃' (equal sets)");
ok(eval('$parents ⊅ $parents'), "infix:'⊅' (equal sets)");
ok(!(eval('$parents ⊋ $parents')), "!infix:'⊋' (equal sets)");

ok(eval('$simpsons ⊇ $parents'), "infix:'⊇'");
ok(!(eval('$parents ⊇ $simpsons')), "!infix:'⊇'");
ok(eval('$parents ⊇ $parents'), "infix:'⊇' (equal sets)");

ok(eval('$simpsons ⊃ ∅'), "infix:'⊃' (empty)");
ok(eval('$parents ⊇ ∅'), "infix:'⊇' (empty)");
ok(!(eval('$simpsons ⊅ ∅')), "infix:'⊅' (empty)");
ok(!(eval('$parents ⊉ ∅')), "infix:'⊉' (empty)");

eval_ok('set(1,2,3) ∋ 1', "infix:<∋>");
eval_ok('1 ∈ set(1,2,3)', "infix:<∈>");
eval_ok('set(1,2,3) ∍ 1', "infix:<∍>");
eval_ok('1 ∊ set(1,2,3)', "infix:<∊>");
eval_ok('!(set(1,2,3) ∌ 1)', "infix:<∌>");
eval_ok('!(1 ∉ set(1,2,3))', "infix:<∉>");

