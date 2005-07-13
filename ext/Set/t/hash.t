#!/usr/bin/pugs

use v6;
use Test;

plan 21;

use Set::Hash;

class Bob {};
class Bert {};

my $bob = Bob.new;
my $bert = Bert.new;

my $set = set(0, 1, 2, 3, $bob);
is(~$set.ref, "Set::Hash", "set()");

ok($set.includes(0), ".includes(0)");
ok($set.includes($bob), ".includes(\$bob)");

is($set.size, 5, ".size");

# .insert() returns the number of items inserted...
is($set.insert(4), 1, ".insert()");
is($set.insert(3), 0, ".insert()");
is($set.insert($bert), 1, ".insert(\$bert)");

is($set.includes(4), bool::true, ".insert() worked");
is($set.member(4),   4,          ".member()");

is($set.includes(5), bool::false, ".includes() - negative");
is($set.includes(4,5), bool::false, ".includes() - mixed negative");
is($set.includes(3,4), bool::true, ".includes() - mixed positive");

is($set.remove($bert), 1, ".remove(\$bert)");

is($set.size, 6, ".size");
# remove also returns the number of elements removed
is($set.remove(4, 5), 1, ".remove");
is($set.size, 5, ".size");

# members returns all the items.  testing this with junctions is maybe
# not thorough enough...
is($set.members, [0, 1, 2, 3, $bob ], ".members()");

$set.clear();

is($set.size(), 0, ".clear()");

# check some aliases...
$set.insert(3,4,5);
is($set.count, 3, ".count()");
is($set.has(4), bool::true, ".has()");

# well, that's a few basic tests, anyway.  Maybe we need a minimal
# sub-class test, too - to make sure the interface works if you only
# define the bare minimum number of methods in a Set sub-class

