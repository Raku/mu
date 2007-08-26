#!/usr/bin/env pugs

use v6;

my @multiples = grep { $^a % 3 == 0 || $^a % 5 == 0 }, 1..^1000;
say sum(@multiples);
