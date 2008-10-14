say "1..1\n";
my $object;
::p6opaque.^!CREATE;
$object.^!how();
#= ::S1P::PurePrototypeHow;
#$object.^!methods.{"foo"} = method {
#    say "ok 1 #method call";
#}
$object.foo;
