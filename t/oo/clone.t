#!/usr/bin/pugs

use v6;
require Test;

plan 9;

# L<S12/"Construction and Initialization" /"ou can clone an object, changing some of the attributes\:"/>
todo_eval_ok 'class Foo { has $.attr = 13 is rw }',
  "basic class definition";

my $a;
todo_eval_ok '$a = Foo.new',  "basic instantiation";
todo_eval_is '$a.attr', 13,   "default attribute";

my $b = $a;
todo_eval_ok '$b =:= $a',     "identity equal";

my $c;
todo_eval_ok '$c = $a.clone', "basic cloning";
todo_eval_is '$c.attr', 13,   "default attribute after cloning";
todo_eval_ok 'not $c =:= $a', "cloning creates objects which are not identity equal";

my $d;
todo_eval_ok '$d = $a.clone(attr => 42)',
  "cloning with supplying a new attribute value";
todo_eval_is '$d.attr', 42,   "changed attribute after cloning";
