knowhow Foo {
    method bar (:$named1, :$named2) {
        $OUT.print($named1.FETCH,"\n"); # the .FETCH here is only
        $OUT.print($named2.FETCH,"\n"); # because of a $OUT bug
    }
}
$OUT.print("1..4\n");
my $foo = "ok 1";
Foo.bar(named1 => $foo, :named2<ok 2>);
my $baz = sub (:$arg) {
    $OUT.print($arg.FETCH,"\n");
};
$baz.(:arg<ok 3>);
$foo = "ok 4";
$baz.(:arg($foo));
