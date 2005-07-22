#!/usr/bin/pugs

use v6;
use Test;

=kwid

Tests assigning default values to variables of type code in sub definitions.

=cut

plan 2;

sub doubler($x) { return 2 * $x }

sub value_v(Code +$func = &doubler) {
	return $func(5);
}

is(value_v, 10, "default sub called");

package MyPack;

use v6;

sub double($x) { return 2 * $x }

sub val_v(Code +$func = &double) is export {
	return $func(5);
}

package main;
use Test;

ok((val_v), "default sub called in package namespace");
