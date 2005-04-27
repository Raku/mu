#!/usr/bin/pugs

use v6;
use Test;

plan 18;

my $foo        = 42;
my $was_inside = 0;

eval 'sub lvalue_test1() is rw {
  $was_inside++;
  return new Proxy:
    FETCH => { 100 + $foo },
    STORE => { $foo = $^new - 100 };
}';

{
    is $foo, 42,       "basic sanity (1)";
    is $was_inside, 0, "basic sanity (2)";

    eval_is 'lvalue_test1()',       142, "getting var through Proxy (1)", :todo;
    # No todo_is here to avoid unexpected succeeds
    is           $was_inside,              1, "lvalue_test1() was called (1)";

    eval_is 'lvalue_test1() = 123', 123, "setting var through Proxy", :todo;
    is           $was_inside,              2, "lvalue_test1() was called (2)";
    is      $foo,                    23, "var was correctly set (1)", :todo;

    eval_is 'lvalue_test1()',       123, "getting var through Proxy (2)", :todo;
    is           $was_inside,              3, "lvalue_test1() was called (3)";
}

$foo        = 4;
$was_inside = 0;

eval 'sub lvalue_test2() is rw {
  $was_inside++;
  return new Proxy:
    FETCH => { 10 + $foo },
    STORE => { $foo = $^new - 100 };
}';

{
    is $foo, 4,        "basic sanity (3)";
    is $was_inside, 0, "basic sanity (4)";

    eval_is 'lvalue_test2()',       14, "getting var through Proxy (4)", :todo;
    # No todo_is here to avoid unexpected succeeds
    is           $was_inside,             1, "lvalue_test2() was called (4)";

    eval_is 'lvalue_test2() = 106', 16, "setting var through Proxy returns new value of the var", :todo;
    is           $was_inside,             2, "lvalue_test2() was called (5)";
    is      $foo,                    6, "var was correctly set (2)", :todo;

    eval_is 'lvalue_test2()',       16, "getting var through Proxy (5)", :todo;
    is           $was_inside,             3, "lvalue_test2() was called (5)";
}
