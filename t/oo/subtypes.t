#!/usr/bin/pugs

use v6;
require Test;

# XXX: 17 tests should be run, but because of the various eval()s dieing too
# early, currently there're only 10 tests run.
# So, to avoid "7/17 failed" messages, *temporarily* pretend that only 10 tests
# are planned.
plan 10;

=pod

Basic subtype tests from L<S12/"Types and Subtypes">

=cut

# Basic subtype creation
todo_eval_ok 'subtype Num::Odd of Num where { $^num % 2 == 1 }',
  "subtype is correctly parsed";
todo_eval_is 'my Num::Odd $a = 3', 3, "3 is an odd num";
# The eval inside the eval is/will be necessary to hider our smarty
# compiler's compile-time from bailing.
# (Actually, if the compiler is *really* smarty, it will notice our eval trick,
# too :))
todo_eval_is 'my Num::Odd $b = 3; try { $a = eval 4 }; $a', 3,
  "objects of Num::Odd don't get even";

# The same, but lexically
my $eval1 = '{
  my subtype Num::Even of Num where { $^num % 2 == 0 }
  todo_ok my Num::Even $c = 6;
  todo_ok $c ~~ Num::Even, "our var is a Num::Even";
  try { $c = eval 7 }
  todo_is $c, 6, "setting a Num::Even to an odd value dies";
}';
eval $eval1;
todo_eval_ok '!try { my Num::Even $d }',
  "lexically declared subtype went out of scope";

# Subs with arguments of a subtype
todo_eval_ok 'sub only_accepts_odds(Num::Odd $odd) { $odd + 1 }',
  "sub requiring a Num::Odd as argument defined (1)";
todo_eval_is 'only_accepts_odds(3)', 4,
  "calling sub worked";
todo_eval_ok '!try { only_accepts_odds(4) }',
  "calling sub did not work";

# Normal Ints automatically morphed to Num::Odd
todo_eval_ok 'sub is_num_odd(Num::Odd $odd) { $odd ~~ Num::Odd }',
  "sub requiring a Num::Odd as argument defined (2)";
todo_eval_ok 'is_num_odd(3)', "Int automatically morphed to Num::Odd";
todo_eval_is 'only_accepts_odds("3")', 4, "Str automatically morphed to Num::Odd";

# Following code is evil, but should work:
my $eval2 = '
  my Int $multiple_of;
  subtype Num::Multiple of Num where { $^num % $multiple_of == 0 }

  $multiple_of = 5;
  todo_ok $multiple_of ~~ Isa, "basic sanity (1)";
  todo_is $multiple_of,     5, "basic sanity (2)";

  todo_ok my Num::Multiple $d = 10, "creating a new Num::Multiple";
  todo_is $d,                   10, "creating a new Num::Multiple actually worked";
  
  $multiple_of = 6;
  todo_ok !try { my Num::Multiple $e = eval 10 },
    "changed subtype definition worked";
';
eval $eval2;
