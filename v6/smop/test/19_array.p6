$*OUT.FETCH.print("1..2\n");
$*array.STORE(::Array.FETCH.new);
$*array.FETCH.{1}.STORE("ok 1\n");
$*OUT.FETCH.print($*array.FETCH.{1}.FETCH);
$*OUT.FETCH.print("ok ",$*array.FETCH.elems,"\n");
