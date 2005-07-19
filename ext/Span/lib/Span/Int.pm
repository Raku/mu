use v6;

# TODO - method difference() for compatibility with Span::Code

class Span::Int-0.01;

has $.start;
has $.end;
has $.density;

submethod BUILD ( $.start, $.end, ?$.density = 1 ) {}

method empty_span ($class: ?$density = 1 ) {
    return $class.new( start => undef, end => undef, density => $density );
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
        return $self.new( start => -Inf, end => $.start - $.density, density => $.density );
    }
    if $.start == -Inf {
        return $self.new( start => $.end + $.density,  end =>   Inf, density => $.density );
    }
    return (   $self.new( start => -Inf, end => $.start - $.density, density => $.density ),
               $self.new( start => $.end + $.density,  end =>   Inf, density => $.density ) );
}

method union ($self: Span::Int $span ) 
    returns List of Span::Int 
{
    return ( $self, $span ) if $.end + $.density     < $span.start;
    return ( $span, $self ) if $span.end + $.density < $.start;
    my $i_start = $.start > $span.start ?? $span.start :: $.start;
    my $i_end =   $.end   < $span.end   ?? $span.end   :: $.end;
    return $self.new( start => $i_start, end =>   $i_end, density => $.density );
}

method intersection ($self: $span ) {

    return $span.intersection( $self )
        if $span.isa( 'Span::Code' ) || $span.isa( 'Span::Num' );

    my $i_start = $.start < $span.start ?? $span.start :: $.start;
    my $i_end =   $.end > $span.end     ?? $span.end   :: $.end;
    return () if $i_start > $i_end;
    return $self.new( start => $i_start, end =>   $i_end, density => $.density );
}

method stringify () returns String {
    return '' unless defined $.start;
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

method difference ($self: $span ) returns List {
    return $self if $self.is_empty;
    my @span = $span.complement;
    @span = @span.map:{ $self.intersection( $_ ) };
    return @span;
}

method next ($self: $x is copy ) {
    $x = $x + $.density if defined $.density;
    return $.start if $x < $.start;
    return $x      if $x <= $.end;
    return undef;
}

method previous ($self: $x is copy ) {
    $x = $x - $.density if defined $.density;
    return $.end   if $x > $.end;
    return $x      if $x >= $.start;
    return undef;
}

=kwid

= NAME

Span::Int - An object representing a single span, with a simple functional API.

= SYNOPSIS

  use Span::Int;

  $span = new( start => $start, end => $end );

= DESCRIPTION

This class represents a single span.

It is intended mostly for "internal" use by the Span class. For a more complete API, see `Span`.

= AUTHOR

Flavio S. Glock, <fglock@pucrs.br>

= COPYRIGHT

Copyright (c) 2005, Flavio S. Glock.  All rights reserved.

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
