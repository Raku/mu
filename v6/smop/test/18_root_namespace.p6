$*OUT.print("1..2\n");
$*bar.STORE("ok 2\n");
$*foo.STORE("ok 1\n");
$*OUT.print($*foo.FETCH);
$*OUT.print($*bar.FETCH);
