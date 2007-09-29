class Parent {
    method ok1 {
        say "ok 1";
    }
}
class Child is Parent {
}
say "1..1";
my $child = Child.new();
$child.ok1();
