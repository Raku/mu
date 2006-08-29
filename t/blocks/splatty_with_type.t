use v6-alpha;

use Test;

plan 1;

sub foo(Int *@array) { ~@array }

is foo(1, 2, 3), "1 2 3", "invoking a splatty sub";
