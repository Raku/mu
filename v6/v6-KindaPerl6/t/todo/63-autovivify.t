use v6-alpha;

say "1..6";

$X::v1 = 1;
say "ok 1 - assignment";

$X::v2 := 1;
say "ok 2 - bind";

$X::v3{1} = 1;
say "ok 3 - assignment to autovivified hash";

$X::v4{1} := 1;
say "ok 4 - bind to autovivified hash";

$X::v5[1] = 1;
say "ok 5 - assignment to autovivified array";

$X::v6[1] := 1;
say "ok 6 - bind to autovivified array";

