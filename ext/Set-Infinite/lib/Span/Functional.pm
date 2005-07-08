use v6;

class Span::Functional-0.01;

has Object $:start;
has Object $:end;
has Bool   $:start_is_open;
has Bool   $:end_is_open;

=for TODO

    * union
        - finish "density" multi method; tests
    * spaceship
        - tests
        - does perl6 uses the "inverted" parameter?

    * complete POD
        - explain "density" parameter

  These methods will not implemented in this class:

    * methods that can be derived from existing methods
        - for example "contains" can use: ( $a->union( $b ) == $a )
    * clone
        - the objects are immutable, they can be reused.

=cut

# constructor: a single constructor, without bounds checking

submethod BUILD ( Object $start, Object $end, Bool $start_is_open, Bool $end_is_open ) 
    returns Span::Functional 
{
    $:start =         $start;
    $:end =           $end;
    $:start_is_open = $start_is_open;
    $:end_is_open =   $end_is_open;
}

multi method size () returns Object {
    return $:end - $:start;
}
multi method size ( Object $density ) returns Object {
    return $:end - $:start - $density 
        if $:start_is_open && $:end_is_open;
    return $:end - $:start 
        if $:start_is_open || $:end_is_open; 
    return $:end - $:start + $density;
}

method start () returns Object {
    return $:start;
}
method end () returns Object {
    return $:end;
}
method start_is_open () returns Bool {
    return $:start_is_open;
}
method start_is_closed () returns Bool {
    return ! $:start_is_open;
}
method end_is_open () returns Bool {
    return $:end_is_open;
}
method end_is_closed () returns Bool {
    return ! $:end_is_open;
}

method intersects ( Span::Functional $span ) returns Bool {
    my ($i_beg, $i_end);
    my Bool $open_beg;
    my Bool $open_end;
    my $cmp = $:start <=> $span.start;
    if ($cmp < 0) {
        $i_beg       = $span.start;
        $open_beg    = $span.start_is_open;
    }
    elsif ($cmp > 0) {
        $i_beg       = $:start;
        $open_beg    = $:start_is_open;
    }
    else {
        $i_beg       = $:start;
        $open_beg    = $:start_is_open || $span.start_is_open;
    }
    $cmp = $:end <=> $span.end;
    if ($cmp > 0) {
        $i_end       = $span.end;
        $open_end    = $span.end_is_open;
    }
    elsif ($cmp < 0) {
        $i_end       = $:end;
        $open_end    = $:end_is_open;
    }
    else {
        $i_end       = $:end;
        $open_end    = $:end_is_open || $span.end_is_open;
    }
    $cmp = $i_beg <=> $i_end;
    return $cmp <= 0  &&
           ( $cmp != 0  ||  ( ! $open_beg && ! $open_end ) );
}

method complement ($self: ) returns List of Span::Functional 
{
    if ($.end == Inf) {
        return if $.start == -Inf;
        return $self.new( start => -Inf,
                     end =>   $:start,
                     start_is_open => bool::true,
                     end_is_open =>   ! $:start_is_open );
    }
    if ($.start == -Inf) {
        return $self.new( start => $:end,
                     end =>   Inf,
                     start_is_open => ! $:end_is_open,
                     end_is_open =>   bool::true );
    }
    return (   $self.new( start => -Inf,
                     end =>   $:start,
                     start_is_open => bool::true,
                     end_is_open =>   ! $:start_is_open ),
               $self.new( start => $:end,
                     end =>   Inf,
                     start_is_open => ! $:end_is_open,
                     end_is_open =>   bool::true ) );
}

multi method union ($self: Span::Functional $span ) 
    returns List of Span::Functional 
{
    my int $cmp;
    $cmp = $:end <=> $span.start;
    if ( $cmp < 0 ||
             ( $cmp == 0 && $:end_is_open && $span.start_is_open ) ) {
        return ( $tmp1, $tmp2 );
    }
    $cmp = $:start <=> $span.end;
    if ( $cmp > 0 ||
         ( $cmp == 0 && $span.end_is_open && $:start_is_open ) ) {
        return ( $tmp2, $tmp1 );
    }

    my ($i_beg, $i_end, $open_beg, $open_end);
    $cmp = $:start <=> $span.start;
    if ($cmp > 0) {
        $i_beg = $span.start;
        $open_beg = $span.start_is_open;
    }
    elsif ($cmp == 0) {
        $i_beg = $:start;
        $open_beg = $:start_is_open ?? $span.start_is_open :: 0;
    }
    else {
        $i_beg = $:start;
        $open_beg = $:start_is_open;
    }

    $cmp = $:end <=> $span.end;
    if ($cmp < 0) {
        $i_end = $span.end;
        $open_end = $span.end_is_open;
    }
    elsif ($cmp == 0) {
        $i_end = $:end;
        $open_end = $:end_is_open ?? $span.end_is_open :: 0;
    }
    else {
        $i_end = $:end;
        $open_end = $:end_is_open;
    }
    return $self.new( start => $i_beg,
                 end =>   $i_end,
                 start_is_open => $open_beg,
                 end_is_open =>   $open_end );
}
multi method union ($self: Span::Functional $span, Object $density ) 
    returns List of Span::Functional 
{
    my int $cmp;

    ...
=for TODO - rewrite this
        my $a1_open =  $.start_is_open ? -$tolerance : $tolerance ;
        my $b1_open =  $.end_is_open   ? -$tolerance : $tolerance ;
        my $a2_open =  $span.start_is_open ? -$tolerance : $tolerance ;
        my $b2_open =  $span.end_is_open   ? -$tolerance : $tolerance ;
        # open_end touching?
        if ((($.end+$.end) + $b1_open ) <
            (($span.start+$span.start) - $a2_open)) {
            # self disjuncts b
            return ( $tmp1, $tmp2 );
        }
        if ((($.start+$.start) - $a1_open ) >
            (($span.end+$span.end) + $b2_open)) {
            # self disjuncts b
            return ( $tmp2, $tmp1 );
        }
=cut

    my ($i_beg, $i_end, $open_beg, $open_end);
    $cmp = $:start <=> $span.start;
    if ($cmp > 0) {
        $i_beg = $span.start;
        $open_beg = $span.start_is_open;
    }
    elsif ($cmp == 0) {
        $i_beg = $:start;
        $open_beg = $:start_is_open ?? $span.start_is_open :: 0;
    }
    else {
        $i_beg = $:start;
        $open_beg = $:start_is_open;
    }

    $cmp = $:end <=> $span.end;
    if ($cmp < 0) {
        $i_end = $span.end;
        $open_end = $span.end_is_open;
    }
    elsif ($cmp == 0) {
        $i_end = $:end;
        $open_end = $:end_is_open ?? $span.end_is_open :: 0;
    }
    else {
        $i_end = $:end;
        $open_end = $:end_is_open;
    }
    return $self.new( start => $i_beg,
                 end =>   $i_end,
                 start_is_open => $open_beg,
                 end_is_open =>   $open_end );
}


method intersection ($self: Span::Functional $span ) 
    returns Span::Functional 
{
    my ($i_beg, $i_end);
    my Bool $open_beg;
    my Bool $open_end;
    my int $cmp = $:start <=> $span.start;
    if ($cmp < 0) {
        $i_beg       = $span.start;
        $open_beg    = $span.start_is_open;
    }
    elsif ($cmp > 0) {
        $i_beg       = $:start;
        $open_beg    = $:start_is_open;
    }
    else {
        $i_beg       = $:start;
        $open_beg    = $:start_is_open || $span.start_is_open;
    }
    $cmp = $:end <=> $span.end;
    if ($cmp > 0) {
        $i_end       = $span.end;
        $open_end    = $span.end_is_open;
    }
    elsif ($cmp < 0) {
        $i_end       = $:end;
        $open_end    = $:end_is_open;
    }
    else {
        $i_end       = $:end;
        $open_end    = $:end_is_open || $span.end_is_open;
    }
    $cmp = $i_beg <=> $i_end;
    if $cmp <= 0  &&
       ( $cmp != 0  ||  ( ! $open_beg && ! $open_end ) )
    {
        return $self.new( start => $i_beg,
                     end =>   $i_end,
                     start_is_open => $open_beg,
                     end_is_open =>   $open_end );
    }
    return;
}

method stringify () returns String {
    my $tmp1 = "$:start";
    my $tmp2 = "$:end";
    return $tmp1 if $tmp1 eq $tmp2;

    return ( $:start_is_open ?? '(' :: '[' ) ~
           $tmp1 ~ ',' ~ $tmp2 ~
           ( $:end_is_open   ?? ')' :: ']' );
}

method spaceship ( Span::Functional $span ) returns Int {
    my int $cmp;
    
    # XXX (perl5) "inverted"?
    # my $inverted = bool::false;
    # if ($inverted) {
    #    $cmp = $span.start <=> $:start;
    #    return $cmp if $cmp;
    #    $cmp = $:start_is_open <=> $span.start_is_open;
    #    return $cmp if $cmp;
    #    $cmp = $span.end <=> $:end;
    #    return $cmp if $cmp;
    #    return $:end_is_open <=> $span.end_is_open;
    # }

    $cmp = $:start <=> $span.start;
    return $cmp if $cmp;
    $cmp = $span.start_is_open <=> $:start_is_open;
    return $cmp if $cmp;
    $cmp = $:end <=> $span.end;
    return $cmp if $cmp;
    return $span.end_is_open <=> $:end_is_open;
}

=kwid

= NAME

Span::Functional - An object representing a single span, with a functional API.

= SYNOPSIS

  use Span:::Functional;

  # XXX

= DESCRIPTION

This class represents a single span.

For a more complete API, see `Span`.

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

= AUTHOR

Flavio S. Glock, <fglock@pucrs.br>

= COPYRIGHT

Copyright (c) 2005, Flavio S. Glock.  All rights reserved.

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
