#!/usr/bin/pugs

use v6;
use Test;

# Tests for sprintf() and .as().

plan 9;

is sprintf("Hi"),                 "Hi",     "sprintf() works with zero args";
is sprintf("%03d",      3),       "003",    "sprintf() works with one arg";
is sprintf("%03d %02d", 3, 1),    "003 01", "sprintf() works with two args";
is sprintf("%d %d %d",  3,1,4),   "3 1 4",  "sprintf() works with three args";
is sprintf("%d%d%d%d",  3,1,4,1), "3141",   "sprintf() works with four args";

eval_is '"Hi".as("[%s]")', "[Hi]", 'as() works with %s', :todo<bug>;

eval_is '"3.141".as("[%d]")', "[3]",  "as() works with %d", :todo<bug>;

# XXX: Following speculative
eval_is '(1.3,2.4,3).as("%d")', "1 2 3", "as() works with lists (speculative test)", :todo<bug>;

# traditionally, sprintf has rounded, not truncated fractions.
is sprintf("%d", 1.5), "2",   "sprintf('%d') rounds arguments", :todo<bug>;


