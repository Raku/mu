$*OUT.FETCH.print("1..3\n");
$*array = ::Array.FETCH.new;
$*array.FETCH.[1] = "ok 1\n";
$*array.FETCH.push("ok 2\n");
$*array.FETCH.unshift("ok 3\n");
$*OUT.FETCH.print($*array.FETCH.[2].FETCH);
$*OUT.FETCH.print($*array.FETCH.[3].FETCH);
$*OUT.FETCH.print($*array.FETCH.[0].FETCH);
$*OUT.FETCH.print("ok ",$*array.FETCH.elems,"\n");
