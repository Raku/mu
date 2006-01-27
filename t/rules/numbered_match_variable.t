#!/usr/bin/pugs

use v6;
use Test;

=pod


=cut

plan 2;

{
        diag "Now going to test numbered match variable.";
        "asdfg/" ~~ rx:perl5{^(\w+)?/(\w+)?}; $1 ?? "true" !! "false";

        ok !$1, "Test the status of non-matched number match variable (1)";
}

{
        "abc" ~~ rx:P5/^(doesnt_match)/;

        ok !$1, "Test the status of non-matched number match variable (2)";
}
