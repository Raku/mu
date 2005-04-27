#!/usr/bin/pugs

use v6;
use Test;

plan 9;

# L<S12/"Construction and Initialization" /you can clone an object, changing some of the attributes:/>
eval_ok 'class Foo { has $.attr = 13 is rw }',
  "basic class definition", :todo;

my $a;
eval_ok '$a = Foo.new',  "basic instantiation", :todo;
eval_is '$a.attr', 13,   "default attribute", :todo;

my $b = $a;
eval_ok '$b =:= $a',     "identity equal", :todo;

my $c;
eval_ok '$c = $a.clone', "basic cloning", :todo;
eval_is '$c.attr', 13,   "default attribute after cloning", :todo;
eval_ok 'not $c =:= $a', "cloning creates objects which are not identity equal", :todo;

my $d;
eval_ok '$d = $a.clone(attr => 42)',
  "cloning with supplying a new attribute value", :todo;
eval_is '$d.attr', 42,   "changed attribute after cloning", :todo;
