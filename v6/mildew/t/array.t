$OUT.print("1..4\n");
my $array = ::Array.new;
$array.[1] = "ok 1\n";
$array.push("ok 2\n");
$array.unshift("ok 3\n");
$OUT.print($array.[2].FETCH);
$OUT.print($array.[3].FETCH);
$OUT.print($array.[0].FETCH);
$OUT.print("ok ",$array.elems.FETCH,"\n");
