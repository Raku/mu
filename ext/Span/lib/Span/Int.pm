use v6;

class Span::Int-0.01;

has $.start;
has $.end;
has $.density;

=for TODO

    * compare
        - tests

    * complete POD
        - explain "density" parameter

    * mark as "internal" class

=cut

submethod BUILD ( $.start, $.end, ?$.density = 1 ) {}

method empty_span ($class: ) {
    return $class.new();
}

method is_empty () { return ! defined( $.start ) }

multi method size () returns Object {
    return $.end - $.start + $.density;
}

method start_is_closed () { return bool::true }
method start_is_open   () { return bool::false }
method end_is_closed   () { return bool::true }
method end_is_open     () { return bool::false }

method intersects ( Span::Int $span ) returns bool {
    my $i_start = $.start < $span.start ?? $span.start :: $.start;
    my $i_end =   $.end > $span.end     ?? $span.end   :: $.end;
    return $i_start <= $i_end;
}

method complement ($self: ) returns List of Span::Int 
{
    if $.end == Inf {
        return () if $.start == -Inf;
        return $self.new( start => -Inf, end => $.start - $.density );
    }
    if $.start == -Inf {
        return $self.new( start => $.end + $.density,  end =>   Inf );
    }
    return (   $self.new( start => -Inf, end => $.start - $.density ),
               $self.new( start => $.end + $.density,  end =>   Inf ) );
}

multi method union ($self: Span::Int $span ) 
    returns List of Span::Int 
{
    return ( $self, $span ) if $.end + $.density     < $span.start;
    return ( $span, $self ) if $span.end + $.density < $.start;
    my $i_start = $.start > $span.start ?? $span.start :: $.start;
    my $i_end =   $.end   < $span.end   ?? $span.end   :: $.end;
    return $self.new( start => $i_start, end =>   $i_end );
}

method intersection ($self: Span::Int $span ) 
    returns Span::Int 
{
    my $i_start = $.start < $span.start ?? $span.start :: $.start;
    my $i_end =   $.end > $span.end     ?? $span.end   :: $.end;
    return () if $i_start > $i_end;
    return $self.new( start => $i_start, end =>   $i_end );
}

method stringify () returns String {
    my $tmp1 = "$.start";
    my $tmp2 = "$.end";
    return $tmp1 if $tmp1 eq $tmp2;
    return '[' ~ $tmp1 ~ ',' ~ $tmp2 ~ ']';
}

method compare ( Span::Int $span ) returns int {
    my int $cmp;
    $cmp = $.start <=> $span.start;
    return $cmp if $cmp;
    return $.end <=> $span.end;
}

=kwid

= NAME

Span::Int - An object representing a single span, with a simple functional API.

= SYNOPSIS

  use Span:::Int;

  # XXX

= DESCRIPTION

This class represents a single span.

For a more complete API, see `Span`.

= CONSTRUCTORS

- `new( start => $start, end => $end )`

- `new( start => $start, end => $end, density => 1 )`

The `start` value must be less than or equal to `end`. There is no checking.

The optional `density` parameter defines the "chunk size". The default density is "1".

= OBJECT METHODS

The following methods are available for Span::Int objects:

- `start()` / `end()`

Return the start or end value.

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

- compare 

  # XXX

= AUTHOR

Flavio S. Glock, <fglock@pucrs.br>

= COPYRIGHT

Copyright (c) 2005, Flavio S. Glock.  All rights reserved.

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
