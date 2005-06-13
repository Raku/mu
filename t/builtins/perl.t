
use v6;

use Test;

# test a bug reported by Chewie[] - apparently this is from S03

plan 1;

is((("f","oo","bar").keys).perl, "(0, 1, 2)", ".perl on a .keys list");


