use v6;

class Set::Span-0.01;

# Set::Span is just like Set::Functional::Span, 
# but it is "mutable" and it has a more complete API

has Set::Functional::Span $.span;

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
    * contains

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

multi submethod BUILD () returns Set::Span {
    undef .$span;
}
multi submethod BUILD ( Object $object ) returns Set::Span {
    $.span = Set::Functional::Span.new( 
        $object, $object, bool::false, bool::false );
}
multi submethod BUILD ( Object $start, Object $end ) returns Set::Span {
    die "start must be less or equal to end" 
        if $start > $end;
    $.span = Set::Functional::Span.new( 
        $start, $end, bool::false, bool::false );
}

method start () returns Object {
    return unless defined $.span;
    return $.span.start;
}
method set_start ( Object $start ) {
    if ! defined $.span {
        $.span = $.span.new( start => $start )
    }
    else
    {
        if $start > $end {
            warn "start must be less or equal to end";
            undef $.span;
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
method set_end ( Object $end ) {
    if ! defined $.span {
        $.span = $.span.new( end => $end )
    }
    else
    {
        if $start > $end {
            warn "start must be less or equal to end";
            undef $.span;
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


=kwid

= NAME

Set::Span - An object representing a single span

= SYNOPSIS

  use Set::Span;

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

The following methods are available for Set::Span objects:

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

If `start` and `end` are times, then `size` is a duration.

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

Returns a Set::Functional::Span object, which may be useful if you
are mixing functional/non-functional programming.

= AUTHOR

Flavio S. Glock, <fglock@pucrs.br>

= COPYRIGHT

Copyright (c) 2005, Flavio S. Glock.  All rights reserved.

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
