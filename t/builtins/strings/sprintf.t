use v6-alpha;

use Test;

plan 6;

# L<S29/Str/"identical to" "C library sprintf">

is sprintf("Hi"),                 "Hi",     "sprintf() works with zero args";
is sprintf("%03d",      3),       "003",    "sprintf() works with one arg";
is sprintf("%03d %02d", 3, 1),    "003 01", "sprintf() works with two args";
is sprintf("%d %d %d",  3,1,4),   "3 1 4",  "sprintf() works with three args";
is sprintf("%d%d%d%d",  3,1,4,1), "3141",   "sprintf() works with four args";

eval_ok('sprintf(%b,1)'),                   'eval of sprintf() with %b';
