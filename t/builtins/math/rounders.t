#!/usr/bin/pugs

use v6;
use Test;

plan 36;

=pod

Basic tests for the round(), floor(), trunc() and ceil() built-ins

=cut

my %tests =
    ( ceiling => [ [ 1.5, 2 ], [ 2, 2 ], [ 1.4999, 2 ],
         [ -0.1, 0 ], [ -1, -1 ], [ -5.9, -5 ],
         [ -0.5, 0 ], [ -0.499, 0 ], [ -5.499, -5 ] ],
      floor => [ [ 1.5, 1 ], [ 2, 2 ], [ 1.4999, 1 ],
         [ -0.1, -1 ], [ -1, -1 ], [ -5.9, -6 ],
         [ -0.5, -1 ], [ -0.499, -1 ], [ -5.499, -6 ]  ],
      round => [ [ 1.5, 2 ], [ 2, 2 ], [ 1.4999, 1 ],
         [ -0.1, 0 ], [ -1, -1 ], [ -5.9, -6 ],
         [ -0.5, -1 ], [ -0.499, 0 ], [ -5.499, -5 ]  ],
      truncate => [ [ 1.5, 1 ], [ 2, 2 ], [ 1.4999, 1 ],
         [ -0.1, 0 ], [ -1, -1 ], [ -5.9, -5 ],
         [ -0.5, 0 ], [ -0.499, 0 ], [ -5.499, -5 ]  ],
    );

if $?PUGS_BACKEND ne "BACKEND_PUGS" {
    skip_rest "PIL2JS and PIL-Run do not support eval() yet.";
    exit;
}

for %tests.keys.sort -> $type {
    my @subtests = %tests{$type};
    for @subtests -> $test {
        my $code = "{$type}($test[0])";
            my $res = eval($code);
        if ($!) {
            flunk("failed to parse $code ($!)", :todo<feature>);
        } else {
            is($res, $test[1], "$code == $test[1]");
        }
    }
}

