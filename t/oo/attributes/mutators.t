
# this tests that you can define mutators, that do more interesting
# things than merely assigning the value!

use Test;

plan 21;

class LValueMutator {
    has Int $.foo;
    has Int $:bar;

    method foo returns Int is rw {
	return $:bar;
    }
    method get_foo returns Int is rw {
        return $.foo;
    }
}

my $lvm = LValueMutator.new(:foo(3));
is($lvm.foo, 3, "constructor uses lvalue accessor method",
   :todo<feature>);
is($lvm.get_foo, undef, "constructor doesn't simply set attributes",
   :todo<feature>);

lives_ok { $lvm.get_foo = 6 }, "lvalue accessors seem to work";
is($lvm.get_foo, 6, "lvalue accessors work");

lives_ok { $lvm.foo = 5 }, "lvalue accessors work still";
is($lvm.foo, 5, "mutator seems to work");

our Int $count = 0;

class MagicVal {
    has Int $.constant;
    has Int $.varies is rw;

    method varies returns Int {
	$count++;
        $.varies += 2;
    }

    # note: there is no specced way of intercepting $foo.varies =
    # $bar; all you can do is make an lvalue accessor.
    method varies(Int $varies) {
	$count+=2;
        $.varies = $varies + 1;
    }
}

my $mv = MagicVal.new(:constant(6), :varies(6));

is($mv.constant, 6, "normal attribute");
is($mv.constant, 6, "normal attribute");
dies_ok { $mv.constant = 7 }, "can't change a non-rw attribute",
	:todo<feature>;
is($mv.constant, 6, "attribute didn't change value",
	:todo<feature>);

is($count, 2, "mutator was called", :todo<feature>);
is($mv.varies, 9, "mutator called during object construction", :todo<feature>);
is($count, 3, "accessor was called", :todo<feature>);
is($mv.varies, 11, "attribute with mutating accessor", :todo<feature>);
is($count, 4, "accessor was called", :todo<feature>);

$count = 0;
$mv.varies(11);
is($count, 2, "mutator was called");
is($mv.varies, 14, "explicit mutator");
is($count, 3, "accessor and mutator were called");

$count = 0;
$mv.varies = 13;
is($count, 2, "mutator was called", :todo<feature>);
is($mv.varies, 16, "attribute with overridden mutator",	
	:todo<feature>);
is($count, 3, "accessor and mutator were called", :todo<feature>);
