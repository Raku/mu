#say "1..1";
$OUT.print("1..1\n");
my $object;
$object = ::p6opaque.^!CREATE;
$object.^!how() = ::PurePrototypeHow;
$object.^!methods.{"foo"} = sub {
    #say "ok 1 #method call";
    $OUT.print("ok 1 #method call\n");
};
$object.foo;
