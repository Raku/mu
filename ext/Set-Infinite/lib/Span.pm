use v6;

class Span-0.01;

# Span is just like Span::Functional, 
# but it is "mutable" and it has a more complete API

use Span::Functional;

has Span::Functional $.span;

=for TODO

    * constructors:
        start/after ..
        .. end/before
        start/after .. end/before
        start/after .. size
        size .. end/before
    * clone
    * size

    * union
    * intersection
    * complement
    * intersects

    * is_empty
    * is_infinite

    * set_start_open / set_start_closed
    * set_end_open / set_end_closed
        - better names ?

From "Set" API:

    * equal/not_equal
    * stringify
    * difference
    * symmetric_difference
    * proper_subset
    * proper_superset
    * subset
    * superset
    * includes/member/has
    * unicode

=cut

multi submethod BUILD () returns Span {
    undefine $.span;
}
multi submethod BUILD ( Object $object ) returns Span {
    $.span = Span::Functional.new( 
        $object, $object, bool::false, bool::false );
}
multi submethod BUILD ( Object $start, Object $end ) returns Span {
    die "start must be less or equal to end" 
        if $start > $end;
    $.span = Span::Functional.new( 
        start => $start, end => $end, start_is_open => bool::false, end_is_open => bool::false );
}

method start () returns Object {
    return unless defined $.span;
    return $.span.start;
}
method set_start ($self: Object $start ) {
    if ! defined( $.span ) 
    {
        $.span = $self.new( start => $start ).span;
    }
    else
    {
        if $start > $end {
            warn "start must be less or equal to end";
            undefine $.span;
        }
        else
        {
            $.span = $.span.new( 
                     start => $start,
                     start_is_open => $.span.start_is_open,
                     end => $.span.end, 
                     end_is_open => $.span.end_is_open );
        }
    }
}

method end () returns Object {
    return $.span.end;
}
method set_end ($self: Object $end ) {
    if ! defined $.span 
    {
        $.span = $self.new( end => $end ).span;
    }
    else
    {
        if $start > $end {
            warn "start must be less or equal to end";
            undefine $.span;
        }
        else
        {
            $.span = $.span.new( 
                     start => $.span.start,
                     start_is_open => $.span.start_is_open,
                     end => $end, 
                     end_is_open => $.span.end_is_open );
        }
    }
}

method start_is_open () returns Bool {
    return $.span.start_is_open;
}
method start_is_closed () returns Bool {
    return $.span.start_is_closed;
}
method end_is_open () returns Bool {
    return $.span.end_is_open;
}
method end_is_closed () returns Bool {
    return $.span.end_is_closed;
}

method contains ($self: Object $span ) returns bool {
    # XXX TODO - the parameter may be a Set::Infinite
    $span = $self.new( $span )
        if ! ( $span.ISA( $self.CLASS ) );
    my @union = $self.span.union( $span.span );
    return bool::false if @union.elems == 2;
    return @union[0].compare( $span.span );
}
method intersects ($self: Object $span ) returns bool {
    # XXX TODO - the parameter may be a Set::Infinite
    $span = $self.new( $span )
        if ! ( $span.ISA( $self.CLASS ) );
    my @union = $self.span.union( $span.span );
    return @union.elems == 1;
}

=for TODO - these methods need Set::Infinite
    * union
    * intersection
    * complement
    * contains
    * difference
=cut

method union ($self: Object $span ) returns Set::Infinite {
    $span = $self.new( $span )
        if ! ( $span.ISA( $self.CLASS ) );
    ...
}
method intersection ($self: Object $span ) returns Set::Infinite {
    $span = $self.new( $span )
        if ! ( $span.ISA( $self.CLASS ) );
    ...
}
method complement ($self: ) returns Set::Infinite {
    ...
}
method difference ($self: Object $span ) returns Set::Infinite {
    $span = $self.new( $span )
        if ! ( $span.ISA( $self.CLASS ) );
    ...
}

=kwid

= NAME

Span - An object representing a single span

= SYNOPSIS

  use Span;

  # XXX

= DESCRIPTION

This class represents a single span.

= CONSTRUCTORS

- `new()`

Without any parameters, returns an empty span.

- `new( start => $start )`

Given a start object, returns a span that has infinite size.

= OBJECT METHODS

    # XXX

The following methods are available for Span objects:

- `start()` / `end()`

Return the start or end value of the span.

These methods may return nothing if the span is empty.

- `set_start( $object )` / `set_end( $object )`

Change the start or end value of the span.

These methods may raise a warning if the new value would put the span 
in an invalid state, such as `start` bigger than `end` (the span is
emptied in this case).

- `start_is_open()` / `end_is_open()` / `start_is_closed()` / `end_is_closed()`

Return a logical value, whether the `start` or `end` values belong to the span ("closed") or not ("open").

- size

Return the "size" of the span.

For example: if `start` and `end` are times, then `size` will be a duration.

- `contains( Object )` / `intersects( Object )`

These methods return a logical value.

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

Returns a Span::Functional object, which may be useful if you
are mixing functional/non-functional programming.

= AUTHOR

Flavio S. Glock, <fglock@pucrs.br>

= COPYRIGHT

Copyright (c) 2005, Flavio S. Glock.  All rights reserved.

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
