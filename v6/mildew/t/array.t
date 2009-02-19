say "1..4";
my $array = ::Array.new;
$array.[1] = "ok 1\n";
$array.push("ok 2\n");
$array.unshift("ok 3\n");
say $array.[2];
say $array.[3];
say $array.[0];
say "ok ",$array.elems,"";
