#!/usr/bin/pugs

use v6;
require Test;

=kwid

Solves A + B = AC

=cut

plan 2;

my $n;
sub is_it($a, $b, $c) {
    $n++;
    if ($a != $b && $b != $c && $a != $c &&
	$a * 10 + $c == $a + $b ) {
	return "$a + $b = $a$c";
    } else {
	return ();
    }
}

my $answer = is_it(any(1..2), any(7..9), any(0..6));
#say "Got:";
#say $answer;
is($n, 42, "called lots of times :-)");

ok( $answer == "1 + 9 = 10", "found right answer");


