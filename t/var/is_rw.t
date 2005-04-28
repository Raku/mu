#!/usr/bin/pugs

use v6;
use Test;

plan 4;

{
  sub foo($a is rw) {
    $a = 42;
    return 19;
  }

  my $bar = 23;
  is $bar,      23, "basic sanity";
  is foo($bar), 19, "calling a sub with an is rw param";
  is $bar,      42, "sub changed our variable";
}

{
	my $anon = -> $a is rw { $a++ };
	my $bar = 10;
	$anon.($bar);
	is($bar, 11, "anon sub changed variable");
}

# for ... -> ... is rw {...} already tested for in t/statements/for.t.
