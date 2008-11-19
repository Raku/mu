knowhow Foo {
    method bar($foo) {
        my $baz = $foo;
        my sub bzzt($boing) {
           if ($boing) {
               $baz = $boing;
           } else {
               $baz = "ok 2\n";
           }
        }
        $OUT.print($baz.FETCH);
        bzzt();
        $OUT.print($baz.FETCH);
        bzzt("ok 3\n");
        $OUT.print($baz.FETCH);
    }
}
$OUT.print("1..3\n");
Foo.bar($baz);