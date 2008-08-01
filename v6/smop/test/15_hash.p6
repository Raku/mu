$*OUT.FETCH.print("1..1\n");
$*hash = ::Hash.FETCH.new;
$*hash.FETCH.{'foo'} = "ok 1\n";
$*OUT.FETCH.print($*hash.FETCH.{'foo'}.FETCH);
