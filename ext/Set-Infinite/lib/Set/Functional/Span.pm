use v6;

class Set::Functional::Span-0.01;

has Object $:start;
has Object $:end;
has Bool   $:start_is_open;
has Bool   $:end_is_open;

=for TODO

    * union
    * intersection
    * contains

From "Set" API (maybe):

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

# constructor: a single constructor, without bounds checking

submethod BUILD ( Object $start, Object $end, Bool $start_is_open, Bool $end_is_open ) 
returns Set::Functional::Span 
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
    if $:start_is_open || $:end_is_open 
    {
        return $:end - $:start - $density if $:start_is_open && $:end_is_open;
        return $:end - $:start;
    }
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

method intersects ( Set::Functional::Span $span ) returns Bool {
    my ($i_beg, $i_end, $open_beg, $open_end);
    my $cmp = $.start <=> $span.start;
    if ($cmp < 0) {
        $i_beg       = $span.start;
        $open_beg    = $span.start_is_open;
    }
    elsif ($cmp > 0) {
        $i_beg       = $.start;
        $open_beg    = $.start_is_open;
    }
    else {
        $i_beg       = $.start;
        $open_beg    = $.start_is_open || $span.start_is_open;
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
    $cmp = $i_beg <=> $i_end;
    return $cmp <= 0  &&
           ( $cmp != 0  ||  ( ! $open_beg && ! $open_end ) );
}

method complement () returns List of Set::Functional::Span 
{
    if ($.end == Inf) {
        return if $.start == -Inf;
        return .new( start => -Inf,
                     end => $.start,
                     start_is_open => bool::true,
                     end_is_open => ! $.start_is_open );
    }
    if ($.start == -Inf) {
        return .new( start => $.end,
                     end => Inf,
                     start_is_open => ! $.end_is_open,
                     end_is_open => bool::true );
    }
    return (   .new( start => -Inf,
                     end => $.start,
                     start_is_open => bool::true,
                     end_is_open => ! $.start_is_open ),
               .new( start => $.end,
                     end => Inf,
                     start_is_open => ! $.end_is_open,
                     end_is_open => bool::true ) );
}

=kwid

= NAME

Set::Functional::Span - An object representing a single span

= SYNOPSIS

  use Set:::Functional::Span;

  # XXX

= DESCRIPTION

This class represents a single span.

= CONSTRUCTORS

- `new( start => $start, end => $end, start_is_open => bool::false, end_is_open => bool::false )`

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
