
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
    has Code $.start;
    has Code $.end;
    class Lazy::Reverse is Lazy::List {
        has $.iter;
        method shift   ( Lazy::Reverse $self: ) { $.iter.pop   }
        method pop     ( Lazy::Reverse $self: ) { $.iter.shift }
        method reverse ( Lazy::Reverse $self: ) { $.iter       }
    }
    submethod BUILD ($class: Coro ?$start is copy, Coro ?$end is copy ) {
        $start //= coro { yield -Inf };
        $end   //= coro { yield  Inf };
    }
    method shift   ( Lazy::List $self: ) { &{$self.start}() }
    method pop     ( Lazy::List $self: ) { &{$self.end}()   }  
    method reverse ( Lazy::List $self: ) { Lazy::Reverse.new( :iter($self) ) }
}

    # XXX - what does (9..1) do?

class Lazy::Range is Lazy::List 
{
    has $.start;
    has $.end;
    has $.step;
    method shift ( Lazy::Range $self: ) {
        my $tmp = $self.start; 
        defined $self.step ?? $self.start += $self.step :: $self.start++;
        return if $tmp > $self.end;
        $tmp;
    }    
    method pop   ( Lazy::Range $self: ) {
        my $tmp = $self.end;
        defined $self.step ?? $self.end -= $self.step :: $self.start--;
        return if $tmp < $self.start;
        $tmp;
    }
}

=kwid

= NAME

Lazy::Range - A lazy list representing a range 

Lazy::List - A lazy list created from coroutines

= SYNOPSIS

  my $iter1 = Lazy::Range.new( start => 10, end => 20 );

  my $iter2 = Lazy::List.new( start => coro mylist2 { yield $_ for 1..3; yield; } );

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
