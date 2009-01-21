#say "1..1";
$OUT.print("1..2\n");
my $object;
$object = ::p6opaque.^!CREATE;
$object.^!how() = ::PrototypeHow;
$object.^!methods.{"foo"} = sub {
    #say "ok 1 #method call";
    $OUT.print("ok 1 #method call\n");
};
$object.^!methods.{"bar"} = sub {
    #say "ok 1 #method call";
    $OUT.print("ok 2 #method inherited from instanceof called\n");
};
$object.foo;
my $object2 = ::p6opaque.^!CREATE;
$object2.^!how() = ::PrototypeHow;
$object2.^!instanceof = $object;
$object2.bar;
