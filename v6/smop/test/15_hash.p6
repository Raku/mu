$*OUT.FETCH.print("1..3\n");

$*hash = ::Hash.FETCH.new;
$*hash.FETCH.{'foo'} = "ok 2\n";
$*hash.FETCH.{'bar'} = "ok 1\n";

$*scalar.STORE(::Scalar.FETCH.new);
$*hash.FETCH.{'baz'}.BIND($*scalar.FETCH);
$*scalar.FETCH.STORE("ok 3\n");
$*OUT.FETCH.print($*hash.FETCH.{'bar'}.FETCH);
$*OUT.FETCH.print($*hash.FETCH.{'foo'}.FETCH);
$*OUT.FETCH.print($*hash.FETCH.{'baz'}.FETCH);
