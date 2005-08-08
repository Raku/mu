
use v6;

# this namespace is from 'S29'
    our &Perl6::Array::pop     := &pop;
    our &Perl6::Array::push    := &push;
    our &Perl6::Array::shift   := &shift;
    our &Perl6::Array::unshift := &unshift;
    our &Perl6::Array::reverse := &reverse;
    our &Perl6::Array::map     := &map;
    our &Perl6::Array::grep    := &grep;

class Lazy::List {
    sub shift   ( Lazy::List $self ) { ... }
    sub pop     ( Lazy::List $self ) { ... }  
    sub reverse ( Lazy::List $self ) { Lazy::Reverse.new( :iter($self) ) }
}
 
class Lazy::Reverse is Lazy::List {
    has $.iter;
    sub shift   ( Lazy::Reverse $self ) { my $x = $self.iter; $x.pop   }
    sub pop     ( Lazy::Reverse $self ) { my $x = $self.iter; $x.shift }
    sub reverse ( Lazy::Reverse $self ) { $self.iter }
}

class Lazy::CoroList is Lazy::List {
    has Code $.start;
    has Code $.end;
    sub shift   ( Lazy::CoroList $self ) { &{$self.start}() }
    sub pop     ( Lazy::CoroList $self ) { &{$self.end}()   }  
    sub reverse ( Lazy::CoroList $self ) { Lazy::Reverse.new( :iter($self) ) }
} 

class Lazy::Range is Lazy::List 
{
    # XXX - what does (9..1) do?
    # XXX - is 'z'..'a' valid?
    has $.start;
    has $.end;
    has $.step;
    sub shift ( Lazy::Range $self ) {
        my $tmp = $self.start; 
        defined $self.step ?? $self.start += $self.step :: $self.start++;
        return if $tmp > $self.end;
        $tmp;
    }    
    sub pop   ( Lazy::Range $self ) {
        my $tmp = $self.end;
        defined $self.step ?? $self.end -= $self.step :: $self.start--;
        return if $tmp < $self.start;
        $tmp;
    }
    # XXX - this should have been inherited, but it wasn't
    sub reverse ( Lazy::Range $self ) { Lazy::Reverse.new( :iter($self) ) }
    
    # XXX
    # sub prefix:<~> () { $.start ~ '..' ~ $.end }
}

=kwid

= NAME

Lazy::Range - A lazy list representing a range 

Lazy::CoroList - A lazy list created from coroutines

Lazy::List - A generic lazy iterator

= SYNOPSIS

...

= DESCRIPTION

...

= CONSTRUCTORS

- `new( :start($a), :end($b), :step(1) )`

= METHODS

- `shift`

- `pop`

- `reverse`

= AUTHOR

Flavio S. Glock, <fglock@gmail.com>

= COPYRIGHT

Copyright (c) 2005, Flavio S. Glock.  All rights reserved.

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
