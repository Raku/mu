use v6-alpha;

say "1..4";

my $v3;
($v3{1}){2} = 1;
say "ok 1 - assignment to autovivified hash";

my $v4;
($v4{1}){2} := 1;
say "ok 2 - bind to autovivified hash";

my $v5;
($v5[1])[2] = 1;
say "ok 3 - assignment to autovivified array";

my $v6;
($v6[1])[2] := 1;
say "ok 4 - bind to autovivified array";

