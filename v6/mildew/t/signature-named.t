knowhow Foo {
    method bar (:$named1, :$named2) {
        say $named1;
        say $named2;
    }
}
say "1..4";
my $foo = "ok 1";
Foo.bar(named1 => $foo, :named2<ok 2>);
my $baz = sub (:$arg) {
    say $arg;
};
$baz(:arg<ok 3>);
$foo = "ok 4";
$baz(:arg($foo));
