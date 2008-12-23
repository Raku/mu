$*OUT.FETCH.print("1..2\n");
$*bar = "ok 2\n";
$*foo = "ok 1\n";
$*OUT.FETCH.print($*foo.FETCH);
$*OUT.FETCH.print($*bar.FETCH);
