use v6;

class Set::Infinite-0.01;

use Span;

has $.set;
has $.density

=for TODO

    * tests

    * union
    * intersection
    * complement
    * intersects
    * compare

    * is_infinite

    * set_start_open / set_start_closed
    * set_end_open / set_end_closed
        - better names ?

    * set_density / density

    * an empty span can have a density
    
    * create a store for single elements (Span::Singleton)
    * is_singleton

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

submethod BUILD ($class: *%param is copy ) {    
    my $density;
    $density = 1 if %param<int>;
    $density = %param<density> if defined %param<density>;

    my @spans;

    for %param<objects> -> $span 
    {
        # TODO - write t/test for Array (such as 1 .. 10 and 1..10,20..30)
        next unless defined( $span );
        $span = Span.new( object => $span );
        push %param<spans>, $span;
    }

    for %param<spans> -> $span 
    {
        next if $span.is_empty;
        $span = $span.span if $span.isa( 'Span' );
        push @spans, $span;
    }
    
    $.set = Set::Infinite::Functional.new();
    $.set = $.set.union( Set::Infinite::Functional.new( spans => $_ ) ) for @spans;
}

method is_empty () returns bool { return $.set.is_empty }

method start () returns Object { return $.set.start }

method set_start ($self: $start ) {
    ...
    if $self.is_empty 
    {
        $.span = $self.new( start => $start ).span;
    }
    else
    {
        my int $cmp = $start <=> $.span.end;
        if $cmp > 0 {
            warn "setting start bigger than end yields an empty set";
            undefine $.span;
        }
        elsif $cmp == 0 && ( $.span.start_is_open || $.span.end_is_open ) {
            warn "setting start equal to end yields an empty set";
            undefine $.span;
        }
        else
        {
            $.span = $.span.clone;
            $.span.start = $start;
        }
    }
}

method end () returns Object { return $.span.end }

method set_end ($self: Object $end ) {
    ...
    if $self.is_empty 
    {
        $.span = $self.new( end => $end ).span;
    }
    else
    {
        my int $cmp = $.span.start <=> $end;
        if $cmp > 0 {
            warn "setting start bigger than end yields an empty set";
            undefine $.span;
        }
        elsif $cmp == 0 && ( $.span.start_is_open || $.span.end_is_open ) {
            warn "setting start equal to end yields an empty set";
            undefine $.span;
        }
        else
        {
            $.span = $.span.clone;
            $.span.end = $end;
        }
    }
}

method start_is_open () returns Bool { return $.set.start_is_open }
method start_is_closed () returns Bool { return $.set.start_is_closed }
method end_is_open () returns Bool { return $.set.end_is_open }
method end_is_closed () returns Bool { return $.set.end_is_closed }
method stringify () returns String { return $.set.stringify }
method size () returns Object { return $.set.size }

method contains ($self: $span is copy) returns bool {
    # XXX TODO - the parameter may be a Set::Infinite
    return bool::false unless defined $.span;
    $span = $self.new( object => $span )
        if ! ( $span.isa( $self.ref ) );
    ...
}

method intersects ($self: $span is copy) returns bool {
    # XXX TODO - the parameter may be a Set::Infinite
    return bool::false unless defined $.span;
    $span = $self.new( object => $span )
        if ! ( $span.isa( $self.ref ) );
    my $span1 = $self.span;
    ...
}

method union ($self: $span ) returns Set::Infinite {
    $span = $self.new( object => $span )
        if ! ( $span.isa( $self.ref ) );
    ...
}
method intersection ($self: $span ) returns Set::Infinite {
    $span = $self.new( object => $span )
        if ! ( $span.isa( $self.ref ) );
    ...
}
method complement ($self: ) returns Set::Infinite {
    ...
}
method difference ($self: $span ) returns Set::Infinite {
    $span = $self.new( object => $span )
        if ! ( $span.isa( $self.ref ) );
    ...
}

=kwid

= NAME

Span::Set - An object representing an ordered set of spans

= SYNOPSIS

  use Span::Set;

  # XXX

= DESCRIPTION

This class represents an ordered set of spans.

= CONSTRUCTORS

- `new()`

Without any parameters, returns an empty span.

- `new( objects => ( 1, 2, 3 ) )`

Creates a span with a single element. This is the same as `new( start => $object, end => $object )`.

- `new( spans => $span )`

Creates a `Set::Infinite` object using an existing span.

- `new( :int, spans => $int_span )`

Creates a set with "integer" semantics. 

The default is "real number" semantics (density = 0).

- `new( spans => $day_span, :density($day_duration) )`

Creates a set with "day" semantics.

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

- compare

  # XXX

- is_empty()

- `spans`

Returns a list of `Span` objects.

= AUTHOR

Flavio S. Glock, <fglock@pucrs.br>

= COPYRIGHT

Copyright (c) 2005, Flavio S. Glock.  All rights reserved.

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
