#!/usr/bin/pugs

use v6;
require Test;

=pod

This tests the &?BLOCK magical from Synoposis 6

L<S06/"The &?BLOCK routine">

L<S06

=cut

plan 1;

# L<S06/"The &?BLOCK routine" /tail-recursion on an anonymous block:$/>
my $anonfactorial = -> Int $n { $n < 2 ?? 1 :: $n * &?BLOCK($n-1) };

my $result = $anonfactorial(3);
is($result, 6, 'the $?BLOCK magical worked');