#!/usr/bin/pugs

use v6;
use Test;

# Tests for sprintf() and .as().

plan 10;

is sprintf("Hi"),                 "Hi",     "sprintf() works with zero args";
is sprintf("%03d",      3),       "003",    "sprintf() works with one arg";
is sprintf("%03d %02d", 3, 1),    "003 01", "sprintf() works with two args";
is sprintf("%d %d %d",  3,1,4),   "3 1 4",  "sprintf() works with three args";
is sprintf("%d%d%d%d",  3,1,4,1), "3141",   "sprintf() works with four args";

eval_is '"Hi".as("[%s]")', "[Hi]", 'as() works with %s';

eval_is '"3.141".as("[%d]")', "[3]",  "as() works with %d";

# L<S02/"Names and Variables" /To format an array value/>
eval_is '(1.3,2.4,3).as("%d", "_")', "1_2_3", "as() works with lists", :todo<feature>;

# L<S02/"Names and Variables" /To format a hash value/>
eval_is 'hash(a => 1.3, b => 2.4).as("%s:%d", "_")', "a:1_b:3", "as() works with hashes", :todo<feature>;

# L<S02/"Names and Variables" /or list of pairs/>
eval_is '(a => 1.3, b => 2.4).as("%s:%d", "_")', "a:1_b:3", "as() works with lists of pairs", :todo<feature>;
