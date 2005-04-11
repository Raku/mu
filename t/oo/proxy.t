#!/usr/bin/pugs

use v6;
require Test;

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

    todo_eval_is 'lvalue_test1()',       142, "getting var through Proxy (1)";
    # No todo_is here to avoid unexpected succeeds
    is           $was_inside,              1, "lvalue_test1() was called (1)";

    todo_eval_is 'lvalue_test1() = 123', 123, "setting var through Proxy";
    is           $was_inside,              2, "lvalue_test1() was called (2)";
    todo_is      $foo,                    23, "var was correctly set (1)";

    todo_eval_is 'lvalue_test1()',       123, "getting var through Proxy (2)";
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

    todo_eval_is 'lvalue_test2()',       14, "getting var through Proxy (4)";
    # No todo_is here to avoid unexpected succeeds
    is           $was_inside,             1, "lvalue_test2() was called (4)";

    todo_eval_is 'lvalue_test2() = 106', 16, "setting var through Proxy returns new value of the var";
    is           $was_inside,             2, "lvalue_test2() was called (5)";
    todo_is      $foo,                    6, "var was correctly set (2)";

    todo_eval_is 'lvalue_test2()',       16, "getting var through Proxy (5)";
    is           $was_inside,             3, "lvalue_test2() was called (5)";
}
