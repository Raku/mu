$*OUT.print("1..1\n");
(sub {
    $*OUT.print("ok 1 - called from sub\n");
}).();
