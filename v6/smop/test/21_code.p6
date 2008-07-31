$*OUT.print("1..3\n");
(sub {
    $*OUT.print("ok 1 - called from sub\n");
}).();
$*msg = "ok 2\n";
$*tmp = sub {$*OUT.print($*msg.FETCH)};
($*tmp).();
$*msg = "ok 3\n";
($*tmp).();
