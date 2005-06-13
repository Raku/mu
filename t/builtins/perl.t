
use v6;

use Test;

# test a bug reported by Chewie[] - apparently this is from S03

plan 1;

is((("f","oo","bar").elems).perl, "(1, 2, 3)", ".perl on a .elems list");


