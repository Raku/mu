use v6;
require Test;

plan 7;

# String Operations
is "text " ~ "stitching", "text stitching", 'concationation with ~ operator';

# Bit Stitching

is 2 || 3, 2, "|| returns first true value";
is eval '2 ?| 3', 1, "boolean or (?|) returns 0 or 1";
ok !(defined( 0 || undef)), "|| returns last false value of list?";
is eval '0 ?| undef', 0, "boolean or (?|) returns 0 or 1";

#junctions

ok all((4|5|6) + 3) == one(7|8|9), "all elements in junction are incremented";

# Hyper ops

skip("waiting for hyper operators");
#is_deeply eval '(1,2,3,4) >>+<< (1,2,3,4)' , (2,4,6,8), 'hyper-add';
