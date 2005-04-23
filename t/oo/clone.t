#!/usr/bin/pugs

use v6;
require Test;

plan 9;

# L<S12/"Construction and Initialization" /you can clone an object, changing some of the attributes:/>
eval_ok 'class Foo { has $.attr = 13 is rw }',
  "basic class definition", :todo(1);

my $a;
eval_ok '$a = Foo.new',  "basic instantiation", :todo(1);
eval_is '$a.attr', 13,   "default attribute", :todo(1);

my $b = $a;
eval_ok '$b =:= $a',     "identity equal", :todo(1);

my $c;
eval_ok '$c = $a.clone', "basic cloning", :todo(1);
eval_is '$c.attr', 13,   "default attribute after cloning", :todo(1);
eval_ok 'not $c =:= $a', "cloning creates objects which are not identity equal", :todo(1);

my $d;
eval_ok '$d = $a.clone(attr => 42)',
  "cloning with supplying a new attribute value", :todo(1);
eval_is '$d.attr', 42,   "changed attribute after cloning", :todo(1);
