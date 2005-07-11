use v6;

class Span::Num-0.01;

has $.start;
has $.end;
has bool   $.start_is_open;
has bool   $.end_is_open;

=for TODO

    * open-set could be "start = object but true" and
      closed-set could be "start = object but false"
    * compare
        - tests
    * complete POD

    * mark as "internal" class

=cut

submethod BUILD ( $.start, $.end, ?$.start_is_open = bool::false, ?$.end_is_open = bool::false ) {}

method empty_span ($class: ) {
    return $class.new( start => undef, end => undef );
}

method is_empty () { return ! defined( $.start ) }

method size () returns Object {
    return $.end - $.start;
}

method start_is_closed () returns bool {
    return ! $.start_is_open;
}

method end_is_closed () returns bool {
    return ! $.end_is_open;
}

method intersects ( Span::Functional $span ) returns bool {
    my ($i_start, $i_end);
    my bool $open_start;
    my bool $open_end;
    my $cmp = $.start <=> $span.start;
    if ($cmp < 0) {
        $i_start       = $span.start;
        $open_start    = $span.start_is_open;
    }
    elsif ($cmp > 0) {
        $i_start       = $.start;
        $open_start    = $.start_is_open;
    }
    else {
        $i_start       = $.start;
        $open_start    = $.start_is_open || $span.start_is_open;
    }
    $cmp = $.end <=> $span.end;
    if ($cmp > 0) {
        $i_end       = $span.end;
        $open_end    = $span.end_is_open;
    }
    elsif ($cmp < 0) {
        $i_end       = $.end;
        $open_end    = $.end_is_open;
    }
    else {
        $i_end       = $.end;
        $open_end    = $.end_is_open || $span.end_is_open;
    }
    $cmp = $i_start <=> $i_end;
    return $cmp <= 0  &&
           ( $cmp != 0  ||  ( ! $open_start && ! $open_end ) );
}

method complement ($self: ) returns List of Span::Functional 
{
    if ($.end == Inf) {
        return () if $.start == -Inf;
        return $self.new( start => -Inf,
                     end =>   $.start,
                     start_is_open => bool::true,
                     end_is_open =>   ! $.start_is_open );
    }
    if ($.start == -Inf) {
        return $self.new( start => $.end,
                     end =>   Inf,
                     start_is_open => ! $.end_is_open,
                     end_is_open =>   bool::true );
    }
    return (   $self.new( start => -Inf,
                     end =>   $.start,
                     start_is_open => bool::true,
                     end_is_open =>   ! $.start_is_open ),
               $self.new( start => $.end,
                     end =>   Inf,
                     start_is_open => ! $.end_is_open,
                     end_is_open =>   bool::true ) );
}

method union ($self: Span::Functional $span ) 
    returns List of Span::Functional 
{
    my int $cmp;
    $cmp = $.end <=> $span.start;
    if ( $cmp < 0 ||
             ( $cmp == 0 && $.end_is_open && $span.start_is_open ) ) {
        return ( $self, $span );
    }
    $cmp = $.start <=> $span.end;
    if ( $cmp > 0 ||
         ( $cmp == 0 && $span.end_is_open && $.start_is_open ) ) {
        return ( $span, $self );
    }

    my ($i_start, $i_end, $open_start, $open_end);
    $cmp = $.start <=> $span.start;
    if ($cmp > 0) {
        $i_start = $span.start;
        $open_start = $span.start_is_open;
    }
    elsif ($cmp == 0) {
        $i_start = $.start;
        $open_start = $.start_is_open ?? $span.start_is_open :: 0;
    }
    else {
        $i_start = $.start;
        $open_start = $.start_is_open;
    }

    $cmp = $.end <=> $span.end;
    if ($cmp < 0) {
        $i_end = $span.end;
        $open_end = $span.end_is_open;
    }
    elsif ($cmp == 0) {
        $i_end = $.end;
        $open_end = $.end_is_open ?? $span.end_is_open :: 0;
    }
    else {
        $i_end = $.end;
        $open_end = $.end_is_open;
    }
    return $self.new( start => $i_start,
                 end =>   $i_end,
                 start_is_open => $open_start,
                 end_is_open =>   $open_end );
}

method intersection ($self: Span::Functional $span ) 
    returns Span::Functional 
{
    my ($i_start, $i_end);
    my bool $open_start;
    my bool $open_end;
    my int $cmp = $.start <=> $span.start;
    if ($cmp < 0) {
        $i_start       = $span.start;
        $open_start    = $span.start_is_open;
    }
    elsif ($cmp > 0) {
        $i_start       = $.start;
        $open_start    = $.start_is_open;
    }
    else {
        $i_start       = $.start;
        $open_start    = $.start_is_open || $span.start_is_open;
    }
    $cmp = $.end <=> $span.end;
    if ($cmp > 0) {
        $i_end       = $span.end;
        $open_end    = $span.end_is_open;
    }
    elsif ($cmp < 0) {
        $i_end       = $.end;
        $open_end    = $.end_is_open;
    }
    else {
        $i_end       = $.end;
        $open_end    = $.end_is_open || $span.end_is_open;
    }
    $cmp = $i_start <=> $i_end;
    if $cmp <= 0  &&
       ( $cmp != 0  ||  ( ! $open_start && ! $open_end ) )
    {
        return $self.new( start => $i_start,
                     end =>   $i_end,
                     start_is_open => $open_start,
                     end_is_open =>   $open_end );
    }
    return;
}

method stringify () returns String {
    return '' unless defined $.start;
    my $tmp1 = "$.start";
    my $tmp2 = "$.end";
    return $tmp1 if $tmp1 eq $tmp2;
    return ( $.start_is_open ?? '(' :: '[' ) ~
           $tmp1 ~ ',' ~ $tmp2 ~
           ( $.end_is_open   ?? ')' :: ']' );
}

method compare ( Span::Functional $span ) returns int {
    my int $cmp;
    $cmp = $.start <=> $span.start;
    return $cmp if $cmp;
    $cmp = $span.start_is_open <=> $.start_is_open;
    return $cmp if $cmp;
    $cmp = $.end <=> $span.end;
    return $cmp if $cmp;
    return $span.end_is_open <=> $.end_is_open;
}

=kwid

= NAME

Span::Functional - An object representing a single span, with a simple functional API.

= SYNOPSIS

  use Span:::Functional;

  $span = new( start => $start, end => $end, start_is_open => bool::false, end_is_open => bool::false );

= DESCRIPTION

This class represents a single span.

It is intended mostly for "internal" use by the Span class. For a more complete API, see `Span`.

= CONSTRUCTORS

- `new( start => $start, end => $end, start_is_open => bool::false, end_is_open => bool::false )`

The `start` value must be less than or equal to `end`. There is no checking.

= OBJECT METHODS

The following methods are available for Span::Functional objects:

- `start()` / `end()`

Return the start or end value.

- `start_is_open()` / `end_is_open()` / `start_is_closed()` / `end_is_closed()`

Return a logical value, whether the `start` or `end` values belong to the span ("closed") or not ("open").

- size

Return the "size" of the span.

If `start` and `end` are times, then `size` is a duration.

- union

- complement

- intersects

- intersection 

- stringify 

- compare 

= AUTHOR

Flavio S. Glock, <fglock@pucrs.br>

= COPYRIGHT

Copyright (c) 2005, Flavio S. Glock.  All rights reserved.

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
