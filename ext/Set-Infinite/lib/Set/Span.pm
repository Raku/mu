use v6;

class Set::Span-0.01;

# Set::Span is just like Set::Functional::Span, 
# but it is "mutable" and it has a more complete API

has Set::Functional::Span $.span;

=for TODO

    * constructors:
        # empty 
        #   Spans can't be empty - use Set::Infinite instead
        # infinite
        #   maybe not worth having a syntax for this
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
method end () returns Object {
    return $.span.end;
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

The following methods are available for Set::Span objects:

- `start()`

Returns the start.

= AUTHOR

Flavio S. Glock, <fglock@pucrs.br>

= COPYRIGHT

Copyright (c) 2005, Flavio S. Glock.  All rights reserved.

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
