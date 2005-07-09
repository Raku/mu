use v6;

class Set::Infinite::Functional-0.01;

use Span::Functional;

has Span::Functional @.spans;

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

submethod BUILD ( Span::Functional @spans ) {
    @.spans = @spans;
}

method start () returns Object {
    return unless @.spans;
    return @.spans[0].start;
}
method end () returns Object {
    return unless @.spans;
    return @.spans[-1].end;
}
method start_is_open () returns bool {
    return bool::false unless @.spans;
    return @.spans[0].start_is_open;
}
method start_is_closed () returns bool {
    return bool::false unless @.spans;
    return @.spans[0].start_is_closed;
}
method end_is_open () returns bool {
    return bool::false unless @.spans;
    return @.spans[-1].end_is_open;
}
method end_is_closed () returns bool {
    return bool::false unless @.spans;
    return @.spans[-1].end_is_closed;
}
method stringify () returns String {
    return '' unless @.spans;
    return @.spans.map:{ .stringify }.join( ',' );
}
method size () returns Object {
    return [+] @.spans.map:{ .size };
}

method intersects ($self: Set::Infinite::Functional $span ) returns bool {
    # XXX TODO 
    ...
    return bool::false unless defined $.span;
    my @union = $self.span.union( $span.span );
    return @union.elems == 1;
}

method union ($self: Set::Infinite::Functional $set ) returns Set::Infinite::Functional {
    # TODO - optional "density"
    # TODO - invert loop order, since the new span is usually "after"
    my @tmp;
    my @res;
    my @a = *@.spans, *$set.spans;
    @a.sort:{ $^a.compare( $^b ) };
    @res[0] = shift @a
        if @a;
    for( @a ) {
        @tmp = @res[-1].union( $_ );
        if @tmp == 2 {
            push @res, @tmp[1];
        }
        else {
            @res[-1] = @tmp[0];
        }
    }
    return $self.new( spans => @res );
}
method intersection ($self: Set::Infinite::Functional $set ) returns Set::Infinite::Functional {
    my $res = $self.new;
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

Set::Infinite::Functional - An object representing an ordered set of spans.

= SYNOPSIS

  use Set::Infinite::Functional;

  # XXX

= DESCRIPTION

This class represents an ordered set of spans.

= CONSTRUCTORS

- `new()`

Creates an empty set.

- `new( Span::Functional @spans )`

Creates a set containing zero or more spans.

The array of spans must be ordered, and the spans must not intersect each other.

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
