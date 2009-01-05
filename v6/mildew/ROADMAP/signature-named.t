knowhow Foo {
    method bar (:$named1, :$named2) {
        $OUT.print($named1.FETCH); # the .FETCH here is only
        $OUT.print($named2.FETCH); # because of a $OUT bug
    }
}
$OUT.print("1..4\n");
Foo.bar(:named1<ok 1\n>, :named2<ok 2\n>);
my $baz = sub (:$arg) {
    $OUT.print($arg.FETCH);
};
$baz.(:arg<ok 3\n>);
my $foo = "ok 4\n";
$baz.(:arg($foo));
