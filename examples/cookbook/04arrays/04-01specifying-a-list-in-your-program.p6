#!/usr/bin/perl6

use v6;

=head1 Specifying a list in your program

You want to include a list in your program.  This is how to initialize arrays.

=cut

# comma separated list of elements
my @a = ('alpha', 'beta', 'gamma');
say @a[1];

# angle brackes to autoquote items
my @a = <alpha beta gamma>;

for @a -> $e {
    say $e;
}
