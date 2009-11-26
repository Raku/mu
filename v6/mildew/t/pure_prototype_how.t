#say "1..1";
say "1..1";
my $object;
$object = ::p6opaque.^!CREATE;
$object.^!how() = ::PrototypeHOW;
$object.^!methods{"foo"} = sub {
    say "ok 1 #method call";
};
#$object.^!methods{"bar"} = sub {
#    say "ok 2 #method inherited from instanceof called";
#};
$object.foo;
my $object2 = ::p6opaque.^!CREATE;
#$object2.^!how() = ::PrototypeHOW;
#$object2.^!instanceof = $object;
#$object2.bar;
