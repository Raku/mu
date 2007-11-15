use v6-alpha;

say "1..6";

my $v1;
$v1 = 1;
say "ok 1 - assignment";

my $v2;
$v2 := 1;
say "ok 2 - bind";

my $v3;
$v3{1} = 1;
say "ok 3 - assignment to autovivified hash";

my $v4;
$v4{1} := 1;
say "ok 4 - bind to autovivified hash";

my $v5;
$v5[1] = 1;
say "ok 5 - assignment to autovivified array";

my $v6;
$v6[1] := 1;
say "ok 6 - bind to autovivified array";

