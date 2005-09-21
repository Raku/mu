#!/usr/bin/pugs

use v6;
use Test;

=pod

The ? case definitely shouldn't be a syntax error.  The next question is
what the correct boolean value is for a Pair, and #perl6 consensus seems
to say that a Pair's boolean value is the boolean value of its, er, value.

=cut

plan 6;

{
    my $true_pair  = 1 => 1;
    my $false_pair = 1 => 0;

    lives_ok { ?$true_pair  }, 'Taking the boolean of a true pair should live';
    lives_ok { ?$false_pair }, 'Taking the boolean of a false pair should live';
    eval_is '?$true_pair',  bool::true,  'A pair with a true value is true';
    eval_is '?$false_pair', bool::false, 'A pair with a false value is false';

    is $true_pair  ?? 1 !! 0, 1, 'Ternary on a true pair returns first option';
    is $false_pair ?? 1 !! 0, 0, 'Ternary on a false pair returns second option';
}
