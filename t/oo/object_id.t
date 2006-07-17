use v6-alpha;

use Test;

plan 1;
# L<A04/"RFC 022: Control flow: Builtin switch statement" /explicit C<\.valid> method in any event/>
class Foo {}

my $num_objects = 20;

my %foos;
for (1 .. $num_objects) {
    my $f = Foo.new();
    %foos{$f.valid()}++;
}

is(+%foos, $num_objects, '... all our .valid()s were unique');
