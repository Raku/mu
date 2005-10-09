#!/usr/bin/pugs

use v6;
use Test;
use FA::DFA;

plan 1;

my FA::DFA $dfa .= new;

ok($dfa,"Object created");
