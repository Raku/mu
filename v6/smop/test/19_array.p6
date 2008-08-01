$*OUT.FETCH.print("1..2\n");
$*array = ::Array.FETCH.new;
$*array.FETCH.[1] = "ok 1\n";
$*OUT.FETCH.print($*array.FETCH.[1].FETCH);
$*OUT.FETCH.print("ok ",$*array.FETCH.elems,"\n");
