#!/usr/bin/pugs

# this tests that you can define mutators, that do more interesting
# things than merely assigning the value!

use v6;
use Test;

plan 25;

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
   :todo<bug>);
is($lvm.get_foo, undef, "constructor doesn't simply set attributes",
   :todo<bug>);

lives_ok { $lvm.get_foo = 6 }, "lvalue accessors seem to work";
is($lvm.get_foo, 6, "lvalue accessors work");

lives_ok { $lvm.foo = 5 }, "lvalue accessors work still";
is($lvm.foo, 5, "mutator seems to work");

our Int $count = 0;

my $parsefail = !eval_ok '
    class MagicVal {
        has Int $.constant;
        has Int $.varies is rw;
    
        method varies returns Int is rw {
        $count++;
        my $var is Proxy( :for($.varies),
                   :FETCH{ $.varies += 2 },
                  :STORE{ $.varies = $_ + 1 },
                     );
        return $var;
        }
    }
', "can parse Proxy trait", :todo<feature>;

if ($parsefail) {
    skip 11, "Proxy trait is parsefail";
} else {

    my $mv = MagicVal.new(:constant(6), :varies(6));

    is($mv.constant, 6, "normal attribute");
    is($mv.constant, 6, "normal attribute");
    dies_ok { $mv.constant = 7 }, "can't change a non-rw attribute",
    :todo<bug>;
    is($mv.constant, 6, "attribute didn't change value",
    :todo<bug>);

    is($count, 2, "mutator was called", :todo<feature>);
    is($mv.varies, 9, "mutator called during object construction",
    :todo<feature>);
    is($count, 3, "accessor was called", :todo<feature>);
    is($mv.varies, 11, "attribute with mutating accessor", :todo<feature>);
    is($count, 4, "accessor was called", :todo<feature>);

    $count = 0;
    $mv.varies = 13;
    is($count, 2, "mutator was called", :todo<feature>);
    is($mv.varies, 16, "attribute with overridden mutator",    
    :todo<feature>);
    is($count, 3, "accessor and mutator were called", :todo<feature>);
}

# test interface tentatively not entirely disapproved of by
# all(@Larry) at http://xrl.us/gnxp
$parsefail = !eval_ok '
    class MagicSub {
        has Int $.constant;
        has Int $.varies is rw;

        method varies returns Int is accessor
            ( :FETCH{ $.varies += 2 },
              :STORE{ $.varies = $^v + 1 } );
    }
', "can parse Proxy trait", :todo<feature>;

if ($parsefail) {
    skip 6, "Proxy trait is parsefail";
} else {

    my $mv = MagicVal.new(:constant(6), :varies(6));

    is($mv.constant, 6, "normal attribute");
    is($mv.constant, 6, "normal attribute");
    dies_ok { $mv.constant = 7 }, "can't change a non-rw attribute",
    :todo<bug>;
    is($mv.constant, 6, "attribute didn't change value",
    :todo<bug>);

    is($mv.varies, 9, "mutator called during object construction",
    :todo<feature>);
    is($mv.varies, 11, "attribute with mutating accessor", :todo<feature>);

    $mv.varies = 13;
    is($mv.varies, 16, "attribute with overridden mutator",    
    :todo<feature>);
}
