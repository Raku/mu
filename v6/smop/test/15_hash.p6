$*OUT.print("1..1\n");
$*hash.STORE(::Hash.new);
$*hash{'foo'}.STORE("ok 1\n");
$*OUT.print($*hash{'foo'}.FETCH);
