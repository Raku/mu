#!/usr/bin/pugs

use v6;
use Test;

plan 7*2;

# L<S06/"Operator overloading">
# Later, we want to run the same tests with two classes, Foo and Bar.
# Foo overloads the operators by using multi methods, Bar by using multi subs.
# But as currently both Foo and Bar do not compile, we have to create a
# stubclass, which is then given to &run_tests_with.
# But if the class does compile, $foo_class and $bar_class will be set to the
# correct classes (Foo and Bar), and the tests have a chance to succeed.
class StubClass {}
my ($foo_class, $bar_class) = (StubClass, StubClass);

eval '
    class Foo {
	has $.bar is rw;
	multi method prefix:<~> ($self)  { return $.bar }
	mutli method infix:<+>  ($a, $b) { return "$a $b" }
    }

    $foo_class = Foo;
';

eval '
    class Bar {
	has $.bar is rw;
    }

    multi sub prefix:<~> (Bar $self)      { return $self.bar }
    mutli sub infix:<+>  (Bar $a, Bar $b) { return "$a $b" }

    $bar_class = Bar;
';

run_tests_with($foo_class);
run_tests_with($bar_class);

sub run_tests_with($class) {
    {
	my $val;
	lives_ok {
	    my $foo = $class.new();
	    $foo.bar = 'software';
	    $val = "$foo"
	}, '... class methods work for class', :todo<feature>;
	is($val, 'software', '... basic prefix operator overloading worked', :todo<feature>);

	lives_ok {
	    my $foo = $class.new();
	    $foo.bar = 'software';
	    $val = $foo + $foo;
	}, '... class methods work for class', :todo<feature>;
	is($val, 'software software', '... basic infix operator overloading worked', :todo<feature>);
    }

    # Test that the object is correctly stringified when it is in an array.
    # And test that »...« automagically work, too.
    {
      my $obj;
      lives_ok {
	  $obj     = $class.new;
	  $obj.bar = "pugs";
      }, "instantiating a class which defines operators worked", :todo<feature>;

      my @foo = ($obj, $obj, $obj);
      is ~@foo, "pugs pugs pugs", "stringification overloading worked in array stringification", :todo<feature>;

      is ~[@foo »~« "!"], "pugs! pugs! pugs!", "stringification overloading was hyperized correctly", :todo<feature>;
    }
}
