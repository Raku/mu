
# 1. test that the receiver utility functions don't exist.

eval "package Foo; use base qw(Perldoc::Receiver);";
is($@,"","Perldoc::Receiver doesn't exist");

# 2. test that the DOM can turn received events into a DOM tree


