#!/usr/bin/pugs

use v6;

require Test;

plan 27;

=head1 DESCRIPITION

These tests test subroutine references and their invocation.

See L<S06/"Types"> for more information about Code, Routine, Sub, Block, etc.

=cut

{
  my $foo = sub () { 42 };
  todo_eval_ok '$foo ~~ Code',    "an anonymous sub isa Code (1)";
  todo_eval_ok '$foo ~~ Routine', "an anonymous sub isa Routine (1)";
  todo_eval_ok '$foo ~~ Sub',     "an anonymous sub isa Sub (1)";
  is $foo.(), 42,                 "basic invocation of an anonymous sub";
  eval_ok '!try { $foo.(23) }',
    "invocation of an parameterless anonymous sub with a parameter dies";
}

{
  my $foo = -> () { 42 };
  todo_eval_ok '$foo ~~ Code',    "a pointy block isa Code";
  todo_eval_ok '$foo ~~ Routine', "a pointy block isa Routine";
  todo_eval_ok '$foo ~~ Block',   "a pointy block isa Block";
  is $foo.(), 42,                 "basic invocation of a pointy block";
  eval_ok '!try { $foo.(23) }',
    "invocation of an parameterless pointy block with a parameter dies";
}

{
  my $foo = { 100 + $^x };
  todo_eval_ok '$foo ~~ Code',    "a parameterized block isa Code";
  todo_eval_ok '$foo ~~ Routine', "a parameterized block isa Routine";
  todo_eval_ok '$foo ~~ Block',   "a parameterized block isa Block";
  is $foo.(42), 142,              "basic invocation of a pointy block with a param";
  eval_ok '!try { $foo.() }',
    "invocation of an parameterized block expecting a param without a param dies";
}

{
  my $foo = sub { 100 + (@_[0] // -1) };
  todo_eval_ok '$foo ~~ Code',    "an anonymous sub isa Code (3)";
  todo_eval_ok '$foo ~~ Routine', "an anonymous sub isa Routine (3)";
  todo_eval_ok '$foo ~~ Sub',     "an anonymous sub isa Sub (3)";
  is $foo.(42), 142,              "basic invocation of a perl5-like anonymous sub (1)";
  is $foo.(),    99,              "basic invocation of a perl5-like anonymous sub (2)";
}

{
  my $foo = sub ($x) { 100 + $x };
  todo_eval_ok '$foo ~~ Code',    "an anonymous sub isa Code (4)";
  todo_eval_ok '$foo ~~ Routine', "an anonymous sub isa Routine (4)";
  todo_eval_ok '$foo ~~ Sub',     "an anonymous sub isa Sub (4)";
  is $foo.(42),      142,    "calling an anonymous sub with a positional param";
  is $foo.(x => 42), 142,    "calling an anonymous sub with a positional param addressed by name";
  eval_ok '!try{ $foo.() }', "calling an anonymous sub expecting a param without a param dies";
  eval_ok '!try{ $foo.(42, 5) }',
    "calling an anonymous sub expecting one param with two params dies";
}
