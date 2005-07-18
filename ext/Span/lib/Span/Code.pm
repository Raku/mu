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
        last if $tmp == @start[-1];
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
    return bool::false if ! $.span.intersects( $span );

    my $tmp = $self.intersection( $span );
    return ! $tmp.is_empty;
}

method union ($set1: $set2 is copy) {

    if ( $set2.isa( 'Span::Num' ) || $set2.isa( 'Span::Int' ) )
    {
        my @diff = $.span.difference( $set2 );
        @diff = @diff.map:{ $set1.new( recurrence => $.recurrence, span => $_ ) };
        return ( @diff, $set2 ).sort{ $^a.compare( $^b ) };
    }

    # XXX - why this doesn't work?
    # my @set1_span = $set1.span.difference( $set2.span );

    my $span1 = $set1.span;
    my $span2 = $set2.span;

    my @set1_span = $span1.difference( $span2 );
    my @set2_span = $span2.difference( $span1 );
    my @intersect = $span1.intersection( $span2 );

    my $union_recurr = $set1.recurrence.union( $set2.recurrence );

    @set1_span = @set1_span.map:{ $set1.new( recurrence => $set1.recurrence, span => $_ ) };
    @set2_span = @set2_span.map:{ $set2.new( recurrence => $set2.recurrence, span => $_ ) };
    @intersect = @intersect.map:{ $set1.new( recurrence => $union_recurr,    span => $_ ) };

    # XXX - why this doesn't work?
    #.sort:{ $^a.span.compare( $^b.span );

    sub _sort ( $x, $y ) { 
        my $s1 = $x.span;
        my $s2 = $y.span;
        return $s1.compare( $s2 );
    };
    my @result = @set1_span, @set2_span, @intersect;
    @result = @result.sort:{ _sort( $^a, $^b ) };
    return @result;
}

method intersection ($set1: $set2 is copy) {

    if ( $set2.isa( 'Span::Num' ) || $set2.isa( 'Span::Int' ) )
    {
        my @inter = $.span.intersection( $set2 );
        @inter = @inter.map:{ $set1.new( recurrence => $.recurrence, span => $_ ) };
        return @inter;
    }

    my @spans = $set1.complement;
    ... 
    return $set1.new( recurrence => $.recurrence, ... );
}

method complement ($self: ) {
    ... 
    return $self.new( recurrence => $.recurrence.complement, ... );
}

method difference ($set1: $set2 is copy ) {
    if ( $span.isa( 'Span::Num' ) || $span.isa( 'Span::Int' ) )
    {
        my @diff = $.span.difference( $set2 );
        @diff = @diff.map:{ $set1.new( recurrence => $.recurrence, span => $_ ) };
        return @diff;
    }
    my @spans = $set1.complement;
    ...
    return $set1.intersection( $set2.complement );
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
