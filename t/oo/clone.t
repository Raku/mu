#!/usr/bin/pugs

use v6;
use Test;

plan 9;

# L<S12/"Construction and Initialization" /you can clone an object, changing some of the attributes:/>
eval_ok 'class Foo { has $.attr = 13 is rw }',
  "basic class definition", :todo<feature>;

my $a;
eval_ok '$a = Foo.new',  "basic instantiation", :todo<feature>;
eval_is '$a.attr', 13,   "default attribute", :todo<feature>;

my $b = $a;
eval_ok '$b =:= $a',     "identity equal", :todo<feature>;

my $c;
eval_ok '$c = $a.clone', "basic cloning", :todo<feature>;
eval_is '$c.attr', 13,   "default attribute after cloning", :todo<feature>;
eval_ok 'not $c =:= $a', "cloning creates objects which are not identity equal", :todo<feature>;

my $d;
eval_ok '$d = $a.clone(attr => 42)',
  "cloning with supplying a new attribute value", :todo<feature>;
eval_is '$d.attr', 42,   "changed attribute after cloning", :todo<feature>;
