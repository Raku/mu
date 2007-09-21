class MyClass {
    has $.foo;
    has $.bar;
};
module Main {
    my $a = MyClass.new();
    $a.foo = 'not ok 1';
    $a.bar = 'not ok 2';

    $a.foo('ok 1');
    $a.bar('ok 2');

    say '1..2';
    say $a.foo;
    say $a.bar;
}