#!/usr/bin/perl6

use v6;

=pod

=head1 Destroying an object

=head2 Problem

You want to run special code when an object is no longer used.

=head2 Solution

Provide a C<DESTROY> method.

=cut

class Foo {
    method DESTROY {
        say "An instance of Foo is going out of scope";
    }
}

my $foo = Foo.new;
