use v6;

=for TODO

    * elements can be spans

    * compare

    * size - accept a function; use as_list

    * as_list

    * contains

    * remove "arbitrary_limit" if possible

    * include Span::Code in Span.pm API

    * stringify - test with 2-5 elements
    * document that stringify doesn't show all elements

    * set_start / set_end

    * remove "difference()" method

=cut

class Span::Code-0.01
{
    has Recurrence $.recurrence;
    has $.span;

submethod BUILD ( $.span, $.recurrence ) {}

method stringify ($self: ) {
    my @start;
    my @end;
    my $samples = 3;
    my $tmp = -Inf;
    for ( 1 .. $samples ) {
        $tmp = $self.next( $tmp );
        push @start, $tmp;
    }
    $tmp = Inf;
    for ( 0 .. $samples ) {
        $tmp = $self.previous( $tmp );
        unshift @end, $tmp;
    }
    return '' if @start[0] > @end[-1];
    return @start[0] if @start[0] == @end[-1];
    # if @start and @end intersect, don't print ".."
    if @end[0] == any( @start ) {
        push @start, @end;
        return @start.uniq.join(',');
    }
    shift @end;
    return @start.uniq.join(',') ~ '..' ~ @end.uniq.join(',');
}

method size () returns Object {
    # TODO - not lazy
    # TODO - empty set
    return undef;
}

method is_empty ($self: ) {
    return ! defined( $self.start );
}

method density () returns Object {
    # TODO - maybe undef is ok
    return undef;
}

method start ($self: ) {
    my $tmp = $self.next( -Inf );
    return $tmp == Inf ?? undef :: $tmp;
}

method end ($self: ) {
    my $tmp = $self.previous( Inf );
    return $tmp == -Inf ?? undef :: $tmp;
}

# the "empty set" test is done by Span.pm
method start_is_closed () { return bool::true }
method start_is_open   () { return bool::false }
method end_is_closed   () { return bool::true }
method end_is_open     () { return bool::false }

method compare ($self: $span is copy) returns int {
    # TODO - this is hard
    ...
}

method contains ($self: $span is copy) returns bool {
    ...
}

method intersects ($self: $span is copy) returns bool {
    # TODO - optimize
    my $tmp = $self.intersection( $span );
    return ! $tmp.is_empty;
}

method union ($self: $span is copy) {
    if ( $span.isa( 'Span::Num' ) || $span.isa( 'Span::Int' ) )
    {
        my $recurrence = $self.recurrence.get_universe();
        $span = $self.new( recurrence => $recurrence, span => $span );
    }
    return $self.new( ... )
}

method intersection ($self: $span is copy) {
    if ( $span.isa( 'Span::Num' ) || $span.isa( 'Span::Int' ) )
    {
        my $recurrence = $self.recurrence.get_universe();
        $span = $self.new( recurrence => $recurrence, span => $span );
    }
    my @spans = $span.complement;
    ... 
    return $self.new( recurrence => $.recurrence, ... );
}

method complement ($self: ) {
    ... 
    return $self.new( recurrence => $.recurrence.complement, ... );
}

method difference ($self: $span is copy ) {
    if ( $span.isa( 'Span::Num' ) || $span.isa( 'Span::Int' ) )
    {
        my $recurrence = $self.recurrence.get_universe();
        $span = $self.new( recurrence => $recurrence, span => $span );
    }
    my @spans = $span.complement;
    ...
    return $self.intersection( $span.complement );
}

method next ($self: $x is copy ) {
    $x = $.recurrence.next( $x );
    if $x < $.span.start && ! $.span.start_is_open {
        $x = $.recurrence.next( $.recurrence.previous( $.span.start ) );
    }
    elsif $x <= $.span.start && $.span.start_is_open {
        $x = $.recurrence.next( $.span.start );
    }
    if $x > $.span.end && ! $.span.end_is_open {
        $x = Inf;
    }
    elsif $x >= $.span.end && $.span.end_is_open {
        $x = Inf;
    }
    return $x;
}

method previous ($self: $x is copy ) {
    $x = $.recurrence.previous( $x );
    if $x > $.span.end && ! $.span.end_is_open {
        $x = $.recurrence.previous( $.recurrence.next( $.span.end ) );
    }
    elsif $x >= $.span.end && $.span.end_is_open {
        $x = $.recurrence.previous( $.span.end );
    }
    if $x < $.span.start && ! $.span.start_is_open {
        $x = -Inf;
    }
    elsif $x <= $.span.start && $.span.start_is_open {
        $x = -Inf;
    }
    return $x;
}


} # class Span::Code


=kwid

= NAME

Span::Code - An object representing a recurrence set span 

= SYNOPSIS

    use Span::Code;

    $span = Span::Code.new( recurrence => $recurrence_set, span => $span );

= AUTHOR

Flavio S. Glock, <fglock@pucrs.br>

= COPYRIGHT

Copyright (c) 2005, Flavio S. Glock.  All rights reserved.

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
