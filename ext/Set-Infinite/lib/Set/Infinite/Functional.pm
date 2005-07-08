use v6;

class Set::Infinite::Functional-0.01;

use Span::Functional;

has Span::Functional @:span;

=for TODO

    * size

    * union
    * intersection
    * complement
    * intersects

    * is_empty
    * is_infinite

    * "density"

=cut

submethod BUILD ($class: Span::Functional $span ) {
    push @:span, $span;
}

method start () returns Object {
    return if @:span.elems == 0;
    return @:span[0].start;
}
method end () returns Object {
    return if @:span.elems == 0;
    return @:span[-1].end;
}
method start_is_open () returns bool {
    return bool::false if @:span.elems == 0;
    return @:span[0].start_is_open;
}
method start_is_closed () returns bool {
    return bool::false if @:span.elems == 0;
    return @:span[0].start_is_closed;
}
method end_is_open () returns bool {
    return bool::false if @:span.elems == 0;
    return @:span[-1].end_is_open;
}
method end_is_closed () returns bool {
    return bool::false if @:span.elems == 0;
    return @:span[-1].end_is_closed;
}
method stringify () returns String {
    return '' if @:span.elems == 0;
    return @:span.join( ',' );
}
method size () returns Object {
    return undef unless defined $.span;
    # XXX TODO
    return @:span.size;
}

method contains ($self: Set::Infinite::Functional $span) returns bool {
    # XXX TODO
    return bool::false unless defined $.span;
    my @union = $self.span.union( $span.span );
    return @union[0].compare( $self.span ) == 0;
}
method intersects ($self: Set::Infinite::Functional $span ) returns bool {
    # XXX TODO 
    return bool::false unless defined $.span;
    my @union = $self.span.union( $span.span );
    return @union.elems == 1;
}

method union ($self: Set::Infinite::Functional $span ) returns Set::Infinite::Functional {
    ...
}
method intersection ($self: Set::Infinite::Functional $span ) returns Set::Infinite::Functional {
    ...
}
method complement ($self: ) returns Set::Infinite::Functional {
    ...
}
method difference ($self: Set::Infinite::Functional $span ) returns Set::Infinite::Functional {
    ...
}

=kwid

= NAME

Set::Infinite::Functional - An object representing an ordered set of spans

= SYNOPSIS

  use Set::Infinite::Functional;

  # XXX

= DESCRIPTION

This class represents an ordered set of spans.

= CONSTRUCTORS

- `new( Span::Functional $span )`

    # XXX

= OBJECT METHODS

    # XXX

The following methods are available for Span objects:

- `start()` / `end()`

Return the start or end value of the span.

These methods may return nothing if the span is empty.

- `start_is_open()` / `end_is_open()` / `start_is_closed()` / `end_is_closed()`

Return a logical value, whether the `start` or `end` values belong to the span ("closed") or not ("open").

- size

Return the "size" of the span.

For example: if `start` and `end` are times, then `size` will be a duration.

- `intersects( Object )`

This method return a logical value.

- union

  # XXX

- complement

  # XXX

- intersects

  # XXX

- intersection 

  # XXX

- stringify 

  # XXX

- spaceship 

  # XXX

- `span`

Returns a Span::Functional object.

= AUTHOR

Flavio S. Glock, <fglock@pucrs.br>

= COPYRIGHT

Copyright (c) 2005, Flavio S. Glock.  All rights reserved.

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
