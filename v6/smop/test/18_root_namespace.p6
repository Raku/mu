$*OUT.print("1..2\n");
$*bar = "ok 2\n";
$*foo = "ok 1\n";
$*OUT.print($*foo.FETCH);
$*OUT.print($*bar.FETCH);
