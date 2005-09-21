#!/usr/bin/pugs

use v6;
use Test;

=pod

The ? case definitely shouldn't be a syntax error.  The next question is
what the correct boolean value is for a Pair, and #perl6 consensus seems
to say that a Pair's boolean value is the boolean value of its, er, value.

=cut

plan 6;

skip_rest "discussion on p6l needed";
exit;

# See thread "Stringification, numification, and booleanification of pairs" on
# p6l started by Ingo Blechschmidt:
# http://www.nntp.perl.org/group/perl.perl6.language/23148

{
    my $true_pair  = 1 => 1;
    my $false_pair = 1 => 0;

    lives_ok { ?$true_pair  }, 'Taking the boolean of a true pair should live';
    lives_ok { ?$false_pair }, 'Taking the boolean of a false pair should live';
    ok  (try { ?$true_pair  }), 'A pair with a true value is true';
    ok !(try { ?$false_pair }), 'A pair with a false value is false';

    is $true_pair  ?? 1 !! 0, 1, 'Ternary on a true pair returns first option';
    is $false_pair ?? 1 !! 0, 0, 'Ternary on a false pair returns second option';
}
