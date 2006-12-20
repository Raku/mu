use v6-alpha;
use Test;
plan 1;

# P12 (**) Decode a run-length encoded list.
# 
# Given a run-length code list generated as specified in problem P11. Construct
# its uncompressed version.

sub decode(*@list) returns Array {
    gather {
        for @list -> $elem {
            take $elem.isa(Array) ?? $elem[1] xx $elem[0] !! $elem;
        }
    }
}
is decode( [4, "a"], "b", [2, "c"], [2, "a"], "d", [4, "e"] ),
    <a a a a b c c a a d e e e e>,
    'We should be able to decode run-length encoded lists';
