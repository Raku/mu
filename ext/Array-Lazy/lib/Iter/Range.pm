use v6;

# XXX - Iter must be a class, because a role can't be tested with 'isa()'
class Iter {
    # this code is from 'Prelude.pm'
    multi sub prefix:<=> (Iter $self: ) { $self.shift() }
    
    sub shift   ( Iter $self ) { ... }
    sub next    ($self: ) { ... }
    sub current ($self: ) { ... }
}
 
class Iter::Range
    is Iter 
{
    has $.start;
    has $.end;
    has $.step;
    sub shift ( Iter::Range $self ) {
        my $tmp = $self.start; 
        $self.start += $self.step;
        return if $tmp > $self.end;
        $tmp;
    }    
    sub pop   ( Iter::Range $self ) {
        my $tmp = $self.end;
        $self.end -= $self.step;
        return if $tmp < $self.start;
        $tmp;
    }
    # XXX
    # sub prefix:<~> () { $.start ~ '..' ~ $.end }
}



=kwid

= NAME

Iter::Range - A lazy range iterator

= SYNOPSIS

...

= DESCRIPTION

...

= CONSTRUCTORS

- `new( :start($a), :end($b), :step(1) )`

= METHODS

- `shift`

- `pop`

= AUTHOR

Flavio S. Glock, <fglock@gmail.com>

= COPYRIGHT

Copyright (c) 2005, Flavio S. Glock.  All rights reserved.

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
