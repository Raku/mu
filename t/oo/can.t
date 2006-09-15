use v6-alpha;

use Test;

=pod

calling can on any object fails. 

=cut

plan 2;

# L<S12/"Introspection"/"But Any gives you shortcuts to those:">

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
