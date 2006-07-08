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

    # XXX Why do we manually declare &length here and then don't add any
    # additional validation code? "has $.length is rw" does this automatically
    # for us.
    method length ($self:) is rw {
        return new Proxy:
            FETCH => { $.length },
            STORE => { $.length = $^length };
    }
}

my $foo = Foo.new;
$foo.length = 13;
say $foo.length;
$foo.length = 'ovid';
say $foo.length;
