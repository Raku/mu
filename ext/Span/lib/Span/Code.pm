use v6;

=for TODO

    * elements can be spans

    * compare

    * size 

    * as_list

    * contains

    * intersects

    * stringify

    * try to make "get_complement_next" work

    * remove "universe" if not used

    * remove "arbitrary_limit" if possible

    * include Span::Code in Span.pm API 

=cut

class Span::Code-0.01
{
    has Code $.closure_next;
    has Code $.closure_previous;
    has Span::Code $.universe;      # may not need this (don't know yet)

    has Code $.complement_next;
    has Code $.complement_previous;

    has $:arbitrary_limit;

submethod BUILD ( $.closure_next, $.closure_previous, ?$complement_next, ?$complement_previous, ?$.universe ) {
    # TODO - get rid of this
    $:arbitrary_limit = 100;
}

method size () returns Object {
    # TODO - not lazy
    return undef;
}

method start () {
    return $.closure_next( -Inf )
}

method end () {
    return $.closure_previous( Inf )
}

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
    ...
}

method union ($self: $span is copy) returns List of Span { 
    return $self.new( 
        closure_next => sub ( $x ) {
                    my $n1 = &{ $self.closure_next }( $x );
                    my $n2 = &{ $span.closure_next }( $x );
                    return $n1 < $n2 ?? $n1 :: $n2;
                },
        closure_previous => sub ( $x ) {
                    my $n1 = &{ $self.closure_previous }( $x );
                    my $n2 = &{ $span.closure_previous }( $x );
                    return $n1 > $n2 ?? $n1 :: $n2;
                },
        complement_next => sub ( $x ) {
                    my $n1;
                    my $n2 = &{ $span.complement_next }( $x );
                    for ( 0 .. $:arbitrary_limit )
                    {
                        $n1 = &{ $self.complement_next }( &{ $self.complement_previous }( $n2 ) );
                        return $n1 if $n1 == $n2;
                        $n2 = &{ $span.complement_next }( &{ $span.complement_previous }( $n1 ) );
                    }
                    warn "Arbitrary limit exceeded when calculating union()";
                },
        complement_previous => sub ( $x ) {
                    my $n1;
                    my $n2 = &{ $span.complement_previous }( $x );
                    for ( 0 .. $:arbitrary_limit )
                    {
                        $n1 = &{ $self.complement_previous }( &{ $self.complement_next }( $n2 ) );
                        return $n1 if $n1 == $n2;
                        $n2 = &{ $span.complement_previous }( &{ $span.complement_next }( $n1 ) );
                    }
                    warn "Arbitrary limit exceeded when calculating union()";
                },
        universe => $self.get_universe,
    )
}

method intersection ($self: $span is copy) returns List of Span {
    return $self.new( 
        closure_next => sub ( $x ) {
                    my $n1;
                    my $n2 = &{ $span.closure_next }( $x );
                    for ( 0 .. $:arbitrary_limit )
                    {
                        $n1 = &{ $self.closure_next }( &{ $self.closure_previous }( $n2 ) );
                        return $n1 if $n1 == $n2;
                        $n2 = &{ $span.closure_next }( &{ $span.closure_previous }( $n1 ) );
                    }
                    warn "Arbitrary limit exceeded when calculating intersection()";
                },
        closure_previous => sub ( $x ) {
                    my $n1;
                    my $n2 = &{ $span.closure_previous }( $x );
                    for ( 0 .. $:arbitrary_limit )
                    {
                        $n1 = &{ $self.closure_previous }( &{ $self.closure_next }( $n2 ) );
                        return $n1 if $n1 == $n2;
                        $n2 = &{ $span.closure_previous }( &{ $span.closure_next }( $n1 ) );
                    }
                    warn "Arbitrary limit exceeded when calculating intersection()";
                },
        complement_next => sub ( $x ) {
                    my $n1 = &{ $self.complement_next }( $x );
                    my $n2 = &{ $span.complement_next }( $x );
                    return $n1 < $n2 ?? $n1 :: $n2;
                },
        complement_previous => sub ( $x ) {
                    my $n1 = &{ $self.complement_previous }( $x );
                    my $n2 = &{ $span.complement_previous }( $x );
                    return $n1 > $n2 ?? $n1 :: $n2;
                },
        universe => $self.get_universe,
    )
}

method complement ($self: ) {
    return $self.new( 
        closure_next =>        &{ $self.get_complement_next }, 
        closure_previous =>    &{ $self.get_complement_previous }, 
        complement_next =>     &{ $self.closure_next },
        complement_previous => &{ $self.closure_previous },
        universe =>            $self.get_universe,
    );
}

method difference ($self: $span is copy) {
    return $self.intersection( $span.complement );
}

method next ( $x ) { 
    # the parent class should verify that $x belongs to the universe set
    return $.closure_next( $x );
}

method previous ( $x ) { 
    return $.closure_previous( $x );
}

method get_complement_next ($self: ) { 

    # XXX this is broken

    return $.complement_next if defined $.complement_next;
    $self.get_universe;
    # say 'Universe starts ', $.universe.start;
    return $.complement_next =
        sub ( $x is copy ) {
            # say 'Complement next ', $x;
            # say 'universe isa ',$.universe.ref;
            # say &{ $.universe.closure_next }( $x );
            for ( 0 .. $:arbitrary_limit )
            {
                $x = &{ $.universe.closure_next }( $x );
say "NEXT current $x";
say "next x ", &{ $self.closure_next }( $x );
say "previous x ", &{ $self.closure_previous }( $x );
                return $x if $x == Inf ||
                             $x != &{ $self.closure_previous }( &{ $self.closure_next }( $x ) );
            }
            warn "Arbitrary limit exceeded when calculating complement()";
        };
}

method get_complement_previous ($self: ) { 

    # XXX this is broken

    return $.complement_previous if defined $.complement_previous;
    $self.get_universe;
say "END ", $self.end;   

    my $end = $self.end;
    my $start = $self.start;

    return $.complement_previous =
        sub ( $x is copy ) {
            for ( 0 .. $:arbitrary_limit )
            {
say "PREVIOUS current $x";
say "next x ", &{ $self.closure_next }( $x );
say "previous x ", &{ $self.closure_previous }( $x );
                $x = &{ $.universe.closure_previous }( $x );
                return $x if $x == -Inf ||
                             $x != &{ $self.closure_next }( &{ $self.closure_previous }( $x ) );
            }
            warn "Arbitrary limit exceeded when calculating complement()";
        };
}

method get_universe ($self: ) {
    return $.universe = $self unless defined $.universe;
    return $.universe;
}

} # class Span::Code


=kwid

= NAME

Span::Code - An object representing spans, defined by a recurrence function

= SYNOPSIS

    use Span::Code;

    $int_span = Span::Code.new( ... );

= AUTHOR

Flavio S. Glock, <fglock@pucrs.br>

= COPYRIGHT

Copyright (c) 2005, Flavio S. Glock.  All rights reserved.

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
