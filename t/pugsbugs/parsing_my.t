#!/usr/bin/pugs

use v6;
use Test;


plan 1;

eval_ok('my $x = my $y = 0; 1', '"my $x = my $y = 0" parses');
