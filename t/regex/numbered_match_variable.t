use v6-alpha;

use Test;

=pod


=cut

plan 2;

{
        diag "Now going to test numbered match variable.";
        "asdfg/" ~~ rx:P5 {^(\w+)?/(\w+)?}; $1 ?? "true" !! "false";

        ok !$1, "Test the status of non-matched number match variable (1)";
}

{
        "abc" ~~ rx:P5/^(doesnt_match)/;

        ok !$1, "Test the status of non-matched number match variable (2)";
}
