$*OUT.FETCH.print("1..1\n");
$*array.STORE(::Array.FETCH.new);
$*array{0}.STORE("ok 1\n");
$*OUT.FETCH.print($*array{0}.FETCH)
