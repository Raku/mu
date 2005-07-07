use v6;

class Set::Span-0.01;

# Dave Rolsky said dot-variables have automatic accessors
# however, these objects should be immutable - maybe these variables should not be dotted.

has Object $.start;
has Object $.end;
has Bool   $.start_is_open;
has Bool   $.end_is_open;

# has Object $.density;
#  probably not a good idea - this is Set::Recurrence business

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

# When density != 0:
#
#    * iterator 
#    * members
#    * count

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

multi submethod BUILD ( $start, $end ) returns Set::Span {
    die "$start must be less or equal to $end" 
        if $start > $end;
    $.start = $start;
    $.end =   $end;
    $.start_is_open = 0;
    $.end_is_open =   0;
}

# method density () returns Object {
#    return $.density;
# }

method start () returns Object {
    return $.start;
}
method end () returns Object {
    return $.end;
}
method start_is_open () returns Bool {
    return $.start_is_open;
}
method start_is_closed () returns Bool {
    return ! $.start_is_open;
}
method end_is_open () returns Bool {
    return $.end_is_open;
}
method end_is_closed () returns Bool {
    return $.end_is_closed;
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
