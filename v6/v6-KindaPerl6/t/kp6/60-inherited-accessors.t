class Parent {
    has $.foo;
    has $.bar;
};
class Child is Parent {
}
module Main {
    my $a = Child.new();
    $a.foo = 'not ok 1';
    $a.bar = 'not ok 2';

    $a.foo('ok 1');
    $a.bar('ok 2');

    say '1..2';
    say $a.foo;
    say $a.bar;
}
