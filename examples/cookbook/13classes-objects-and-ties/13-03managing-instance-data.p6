#!/usr/bin/perl6

use v6;

=pod

=head1 Managing instance data

=head2 Problem

Each instance of an object can have its own, private data.  How do you 
write methods to manage that data?

=head2 Solution



=cut

class Foo {
    has int $.length is rw;
    has $.that;

    multi method length ($self: $length) {
        $.length = $length;
        return $self;
    }

    multi method length($self) {
        return $.length;
    }
}

my $foo = Foo.new;
$foo.length = 13;
say $foo.length;
$foo.length = 'ovid';
say $foo.length;
