use Test; plan 9;

# L<S12/"Calling Sets of Methods">

# Spec:
# For any method name, there may be some number of candidate methods that could
# handle the request: typically, inherited methods or multi variants. The
# ordinary "dot" operator dispatches to a method in the standard fashion. There
# are also "dot" variants that call some number of methods with the same name:

#      $object.?meth  # calls method if there is one, otherwise undef
class Parent {
    has Int $.cnt is rw;
    method meth {$.cnt++}
}
class Child is Parent {
    method meth {$.cnt++}
    method child_only {'child_only'}
}

{
    my $test = q"$object.?meth calls method is there one";
    my $object = Child.new;
    my $result = 1; # default to one to see if value changes to undef
    try { $result = $object.?nope };
    ok($object.?meth, $test);
    is($result,undef, q"                                       ..undef otherwise ");
}

{
    my $test = q"$object.*meth(@args)  # calls all methods (0 or more)";
    my $object = Child.new;
    my $result = 1; # default to one to see if value changes to undef
    try { $result = $object.*nope };
    is($result,undef, q"$test: Case 0 returns undef");

    try { $result = $object.*child_only };
    is($result, 'child_only', "$test: Case 1 fines one result"); 

    try { $result = $object.*meth };
    is($object.cnt, 2, "$test: Case 2 visits both Child and Parent");

}

{
    my $test = q"$object.+meth(@args)  # calls all methods (1 or more)";
    my $object = Child.new;
    my $result = 1; # default to one to see if value changes to undef
    try { $result = $object.+nope };
    ok($!, q"$test: Case 0 dies");

    try { $result = $object.+child_only };
    is($result, 'child_only', "$test: Case 1 fines one result"); 

    try { $result = $object.+meth };
    is($object.cnt, 2, "$test: Case 2 visits both Child and Parent");

}

ok(0, "STUB: there is more Calling Sets functionality which needs tests", :todo<feature>);

