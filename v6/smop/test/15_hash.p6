$io.print("1..2\n");
$io.print("ok 1\n");
$hash{'foo'}.STORE("ok 2\n");
$io.print($hash{'foo'}.FETCH);
