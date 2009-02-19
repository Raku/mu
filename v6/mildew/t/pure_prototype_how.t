#say "1..1";
say "1..2";
my $object;
$object = ::p6opaque.^!CREATE;
$object.^!how() = ::PrototypeHow;
$object.^!methods.{"foo"} = sub {
    #say "ok 1 #method call";
    say "ok 1 #method call";
};
$object.^!methods.{"bar"} = sub {
    #say "ok 1 #method call";
    say "ok 2 #method inherited from instanceof called";
};
$object.foo;
my $object2 = ::p6opaque.^!CREATE;
$object2.^!how() = ::PrototypeHow;
$object2.^!instanceof = $object;
$object2.bar;
