#!/usr/bin/pugs

use v6;
use Test;

plan 23;

ok(1 =:= 1, "int");
ok(!(2 =:= 1), "int (neg)");
ok("foo" =:= "foo", "str");
ok(!("foo" =:= "foe"), "str (neg)");

ok(("7" == 7), "sanity");
ok(("7" eq 7), "sanity");
ok(!("7" =:= 7), "identify checks type mismatch");

{
  my $foo = 1;
  my $bar = 1;
  ok($foo =:= $foo, "int in one scalar");
  ok($foo =:= $bar, "int in two scalars");

  eval_ok('$foo = 1 but false', :todo<feature>);
  ok($foo == $bar, "sanity");
  ok(!($foo =:= $bar), "being an object makes it not identical",
	:todo<feature>);
}

class TestObj {
   has $:a;
};

{
  my $foo = TestObj.new(:a<3>);
  my $bar = TestObj.new(:a<3>);
  my $baz = $foo;
  my $frop := $foo;

  ok(!($foo =:= $bar), "two identical objects are not the same object");
  ok(($foo =:= $baz), "two references to one object are the same object");
  ok(($foo =:= $frop), "binding makes two objects the same object");

  my $test = sub($arg) {
     return ($foo =:= $arg); 
  };

  ok($test($foo), "binding via -> retains identity");
  ok(!$test($bar), "..");
  ok($test($baz), "..");
  ok($test($frop), "..");

  $test = sub {
     return ($foo =:= @_[0]);
  };

  ok($test($foo), "binding via @_ retains identity");
  ok(!$test($bar), "..");
  ok($test($baz), "..");
  ok($test($frop), "..");
}

