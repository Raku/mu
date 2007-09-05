grammar Test {
}
say "1..2";
&foo := token :Perl5 {foo}
(Test.HOW).add_method('foo',&foo);
if (Test.foo("foo")) {
    say "ok 1";
} else {
    say "not ok 1";
}
if (Test.foo("bar")) {
    say "not ok 2";
} else {
    say "ok 2";
}
