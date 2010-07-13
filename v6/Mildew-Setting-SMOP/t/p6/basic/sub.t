say "1..4";
sub foo($foo) {
    say $foo;
}
foo("ok 1");
foo "ok 2";
&foo.("ok 3");
&foo("ok 4");
