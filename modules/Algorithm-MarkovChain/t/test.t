#!/usr/bin/perl6

use Test;
BEGIN { plan 15 }
use Algorithm::MarkovChain;

my Algorithm::MarkovChain $mc .= new();
ok $mc ~~ Algorithm::MarkovChain;

seed $mc: symbols => <a b>;

is_deeply $mc.get_options("a"), { b => 1 }, "known options";

is $mc.longest_sequence, 1,  "longest sequence";
is $mc.random_sequence, "a", "single random sequence";
ok $mc.sequence_known("a"),  "sequence known";

is_deeply
  $mc.spew(:stop_at_terminal, :complete(<a>),
  <a b>,
  "complete known";
