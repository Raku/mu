use v6-pugs;

use Test;

=pod

We don't have references yet, but that doesn't mean dereferencing should
be a nop.

=cut

plan 1;
my $x = 42;
dies_ok { $$$$$$$$$$x }, "can't endlessly dereference", :todo<bug>;

# (currently this evaluates to 42)
