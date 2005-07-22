use v6;

class Set::Infinite::Functional-0.01;

use Span;

has @.spans;

=for TODO

    * compare

=cut

submethod BUILD ( @.spans ) {}

method empty_set ($class: ?$density ) returns Set::Infinite::Functional {
    $class.new( spans => () );
}

method universal_set ($class: ) returns Set::Infinite::Functional {
    $class.new( spans => Span.new( start => -Inf, end => Inf ) );
}

method is_empty () returns bool { return ! @.spans }

method is_infinite ($self: ) returns bool {
    return $self.start == -Inf || $self.end == Inf
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

method union ($self: Set::Infinite::Functional $set ) 
    returns Set::Infinite::Functional 
{
    # TODO - optimize; invert loop order, since the new span is usually "after"
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
    # TODO - optimize
    my @res;
    my @a = @.spans;
    my @b = $set.spans;
    while @a && @b {
        push @res, @a[0].intersection( @b[0] );
        if @a[0].end < @b[0].end { shift @a } else { shift @b }
    }
    return $self.new( spans => @res );
}

method intersects ( Set::Infinite::Functional $set ) returns bool {
    # TODO - optimize
    my @res;
    my @a = @.spans;
    my @b = $set.spans;
    while @a && @b {
        return bool::true if @a[0].intersection( @b[0] );
        if @a[0].end < @b[0].end { shift @a } else { shift @b }
    }
    return bool::false;
}

method complement ($self: ) returns Set::Infinite::Functional {
    return $self.universal_set( density => $self.density ) 
        if $self.is_empty;
    return @.spans.map:{ $self.new( spans => $_.complement ) }
                  .reduce:{ $^a.intersection( $^b ) };
}

method difference ($self: Set::Infinite::Functional $span ) returns Set::Infinite::Functional {
    return $self.intersection( $span.complement );
}

method compare ($self: Set::Infinite::Functional $span ) returns int {
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

It is intended mostly for "internal" use by the Set::Infinite class. For a more complete API, see `Set::Infinite`.

= CONSTRUCTORS

- `new()`

Creates an empty set.

- `new( spans => @spans )`

Creates a set containing zero or more `Span::Int` or `Span::Num` span objects.

The array of spans must be ordered, and the spans must not intersect with each other.

- empty_set( density => $d )

- universal_set( density => $d )

= OBJECT METHODS

    # XXX

The following methods are available for Span objects:

- `start()` / `end()`

Return the start or end value of the span.

These methods may return nothing if the span is empty.

- `start_is_open()` / `end_is_open()` / `start_is_closed()` / `end_is_closed()`

Return a logical value, whether the `start` or `end` values belong to the span ("closed") or not ("open").

- size()

Return the "size" of the span.

For example: if `start` and `end` are times, then `size` will be a duration.

- `intersects( $set )`

This method return a logical value.

- union( $set )

  # XXX

- complement()

  # XXX

- intersects( $set )

  # XXX

- intersection( $set ) 

  # XXX
  
- difference( $set )

  # XXX

- stringify() 

  # XXX

- compare

  # XXX
  
- is_empty()

- is_infinite()

- `spans()`

Returns a list of `Span::Int` or `Span::Num` objects.

- `density()`

  # XXX

= AUTHOR

Flavio S. Glock, <fglock@pucrs.br>

= COPYRIGHT

Copyright (c) 2005, Flavio S. Glock.  All rights reserved.

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
