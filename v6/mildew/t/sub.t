say "1..3";
sub foo($foo) {
    say $foo;
}
foo("ok 1\n");
foo "ok 2\n";
&foo.("ok 3\n");
