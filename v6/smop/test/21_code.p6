$*OUT.FETCH.print("1..3\n");
(sub {
    $*OUT.FETCH.print("ok 1 - called from sub\n");
}).();
$*msg = "ok 2\n";
$*tmp = sub {$*OUT.FETCH.print($*msg.FETCH)};
($*tmp.FETCH).();
$*msg = "ok 3\n";
($*tmp).();
