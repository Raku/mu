#!/usr/bin/pugs

use v6;
use Test;

=pod
pugs> "asdfg/" ~~ rx:perl5{^(\w+)?/(\w+)?}; $1 ?? "true" !! "false"
"true"

$1 didn't match anything, But the in boolean context, It is treated as true.

=cut

plan 1;

{
        ok "Now going to test numbered match variable.";
        "asdfg/" ~~ rx:perl5{^(\w+)?/(\w+)?}; $1 ?? "true" !! "false";

        isnt ?$1, ?1, "Test the status of non-matched number match variable";
}
