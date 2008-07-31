$*OUT.print("1..3\n");
(sub {
    $*OUT.print("ok 1 - called from sub\n");
}).();
$*msg.STORE("ok 2\n");
$*tmp.STORE(sub {$*OUT.print($*msg)});
($*tmp).();
$*msg.STORE("ok 3\n");
($*tmp).();
