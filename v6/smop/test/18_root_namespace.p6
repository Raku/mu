$io.print("1..2\n");
$*bar.STORE("ok 2\n");
$*foo.STORE("ok 1\n");
$io.print($*foo.FETCH);
$io.print($*bar.FETCH);
