#!/usr/bin/pugs

use v6;
use Test;

=pod

calling can on any object fails. 

=cut

plan 2;

class Dog {
        method bark {
                "bow";
        }
}

lives_ok {
        my $dog = Dog.new;
        $dog.can("bark");
}, "method can on custom class fails", :todo<bug>;

lives_ok { Str.can("split") }, "method can on built-in Str fails", :todo<bug>;
