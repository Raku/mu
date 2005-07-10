#!/usr/bin/pugs

use v6;
use Test;

plan 28;

=pod

Basic tests for the round(), floor(), trunc() and ceil() built-ins

=cut

my %tests =
    ( ceil  => [ [ 1.5, 2 ], [ 2, 2 ], [ 1.4999, 2 ],
		 [ -0.1, 0 ], [ -1, -1 ], [ -0.9, 0 ],
		 [ -0.5, 0 ] ],
      floor => [ [ 1.5, 1 ], [ 2, 2 ], [ 1.4999, 1 ],
		 [ -0.1, -1 ], [ -1, -1 ], [ -0.9, -1 ],
		 [ -0.5, -1 ] ],
      round => [ [ 1.5, 2 ], [ 2, 2 ], [ 1.4999, 1 ],
		 [ -0.1, 0 ], [ -1, -1 ], [ -0.9, -1 ],
		 [ -0.5, -1 ] ],
      trunc => [ [ 1.5, 1 ], [ 2, 2 ], [ 1.4999, 1 ],
		 [ -0.1, 0 ], [ -1, -1 ], [ -0.9, 0 ],
		 [ -0.5, 0 ] ],
    );
      
for %tests.keys.sort -> $type {
    my @subtests = %tests{$type};
    for @subtests -> $test {
	my $code = "{$type}($test[0])";
        my $res = eval($code);
	if ($!) {
	    fail("failed to parse $code ($!)", :todo<feature>);
	} else {
	    is($res, $test[1], "$code == $test[1]");
	}
    }
}

