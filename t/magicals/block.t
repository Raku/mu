#!/usr/bin/pugs

use v6;
require Test;

plan 1;

# example taken from Synopsis 6

my $anonfactorial = -> Int $n { $n < 2 ?? 1 :: $n * &?BLOCK($n-1) };

my $result = $anonfactorial(3);
is($result, 6, 'the $?BLOCK magical worked');