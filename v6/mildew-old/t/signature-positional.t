knowhow Foo {
    method bar ($positional1, $positional2) {
        say $positional1;
        say $positional2;
    }
}
say "1..4";
Foo.bar("ok 1\n", "ok 2\n");
my $baz = sub ($arg) {
    say $arg;
};
$baz("ok 3\n");
my $foo = "ok 4\n";
$baz($foo);
