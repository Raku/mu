$*OUT.print("1..2\n");
$*array.STORE(::Array.new);
$*array[1].STORE("ok 1\n");
$*OUT.print($*array[1].FETCH);
$*OUT.print("ok ",$*array.elems,"\n");
