use v6;

use Span;
use Set::Symbols;
use Set::Infinite::Functional;

class Set::Infinite-0.01
    does Set::Symbols;

has Set::Infinite::Functional $.set;

=for TODO

    * tests
    * test recurrences

    * compare

    * set_start_open / set_start_closed
    * set_end_open / set_end_closed
        - better names ?

    * is_singleton

    * "backtracking" - see Perl5 version - recurrence of spans

    * first_span/last_span; span_iterator; iterator

    * add_spans / remove_spans mutators

From "Set" API:

    * equal/not_equal
    * symmetric_difference
    * proper_subset
    * proper_superset
    * subset
    * superset
    * includes/member/has

=cut

submethod BUILD ($class: *%param ) {    
    my @spans;
    for ( *%param<objects>, *%param<spans> ) -> $span
    {
        # TODO - write t/test for Array (such as 1 .. 10 and 1..10,20..30)
        next unless defined( $span );
        my $sp = Span.new( object => $span );
        next if $sp.is_empty;
        push @spans, $sp;
    }
    # TODO - use reduce()
    $.set = Set::Infinite::Functional.empty_set();
    $.set = $.set.union( Set::Infinite::Functional.new( spans => $_ ) ) for @spans;
}

method universal_set ($class: ) returns Set::Infinite { 
    $class.new( spans => Span.new( start => -Inf, end => Inf ) );
}
method empty_set ($class: ) returns Set::Infinite { $class.new() }

method spans ()         returns List of Span { return $.set.spans }
method is_empty ()      returns bool   { return $.set.is_empty }
method is_infinite ()   returns bool   { return $.set.is_infinite }
method start ()         returns Object { return $.set.start }
method end ()           returns Object { return $.set.end }
method start_is_open () returns Bool   { return $.set.start_is_open }
method start_is_closed () returns Bool { return $.set.start_is_closed }
method end_is_open ()   returns Bool   { return $.set.end_is_open }
method end_is_closed () returns Bool   { return $.set.end_is_closed }
method stringify ()     returns String { return $.set.stringify }
method size ()          returns Object { return $.set.size }

submethod normalize_parameter ($self: $span) {
    # is it a Set::Infinite ?
    return $span.set if $span.isa( $self.ref );
    # is it a Set::Infinite::Functional ?
    my $span0 = $self.set;
    return $span if $span.isa( $span0.ref );
    # Span.pm knows what to do: Span, Span::Num, Span.Int, Object
    my $span = Span.new( object => $span );
    return $span0.new( spans => $span );
}

method compare ($self: $span is copy) {
    my $span0 = $self.set;
    my $span1 = $self.normalize_parameter( $span );
    return $span0.compare( $span1 );
}

method contains ($self: $span is copy) returns bool {
    return bool::false if $.span.is_empty;
    my $span0 = $self.set;
    my $span1 = $self.normalize_parameter( $span );
    my $union = $span0.union( $span1 );
    return $span0.compare( $union ) == 0;
}

method intersects ($self: $span is copy) returns bool {
    my $span0 = $self.set;
    my $span1 = $self.normalize_parameter( $span );
    return $span0.intersects( $span1 );
}

method union ($self: $span ) returns Set::Infinite {
    my $span0 = $self.set;
    my $span1 = $self.normalize_parameter( $span );
    my $tmp = $span0.union( $span1 );
    
    my $res = Set::Infinite.new();
    $res.set = $tmp;
    return $res;
}
method intersection ($self: $span ) returns Set::Infinite {
    my $span0 = $self.set;
    my $span1 = $self.normalize_parameter( $span );
    my $tmp = $span0.intersection( $span1 );
    my $res = Set::Infinite.new();
    $res.set = $tmp;
    return $res;
}
method complement ($self: ) returns Set::Infinite {
    my $span0 = $self.set;
    my $tmp = $span0.complement;
    my $res = Set::Infinite.new();
    $res.set = $tmp;
    return $res;
}
method difference ($self: $span ) returns Set::Infinite {
    my $span0 = $self.set;
    my $span1 = $self.normalize_parameter( $span );
    my $tmp = $span1.complement;
    $tmp = $span0.intersection( $tmp );
    my $res = Set::Infinite.new();
    $res.set = $tmp;
    return $res;
}

=kwid

= NAME

Set::Infinite - An object representing an ordered set of spans

= SYNOPSIS

  use Set::Infinite;

  my $set = Set::Infinite.new( spans => $span );

= DESCRIPTION

This class represents an ordered set of spans.

The problem this class solves is when you need do perform "set" operations on 
Span objects, that return lists of spans.

In this example, @span may contain zero, one, two, or three spans, depending on
the contents of $span1 and $span2:

  @span = $span1.union( $span2 );
  say @span.map:{ .stringify }.join(",");

On the other hand, a Set::Infinite object is more practical to work with:

  $set = $set1.union( $set2 );
  say $set.stringify;

= CONSTRUCTORS

- `new()`

Without any parameters, returns an empty span.

- `new( objects => ( 1, 3, 9 ) )`

Creates a `Set::Infinite` object with a few elements.

- `new( spans => $span )`

Creates a `Set::Infinite` object using an existing span.

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
