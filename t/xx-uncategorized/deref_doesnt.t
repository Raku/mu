use v6;

use Test;

=begin pod

We don't have references yet, but that doesn't mean dereferencing should
be a nop.

=end pod

plan 1;
my $x = 42;
dies_ok { $$$$$$$$$$x }, "can't endlessly dereference", :todo<bug>;

# (currently this evaluates to 42)
