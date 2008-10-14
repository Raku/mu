say "1..1\n";
my $object;
$object = ::p6opaque.^!CREATE;
$object.^!how() = ::S1P::PurePrototypeHow;
$object.^!methods.postcircumfix:<{ }>("foo") = sub {
    say "ok 1 #method call";
};
$object.foo;
