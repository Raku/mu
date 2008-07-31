$*OUT.print("1..1\n");
$*hash = ::Hash.new;
$*hash{'foo'} = "ok 1\n";
$*OUT.print($*hash{'foo'}.FETCH);
