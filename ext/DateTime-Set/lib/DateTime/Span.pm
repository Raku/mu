use v6;

class DateTime::Span-0.01
    does Span;

# XXX - is this needed?
use Span;

=for TODO

    * change returned 'Inf' to 'DateTime::Infinite'

    * from_duration constructor needs some tweaks

    * tests

    * document differences from Perl5 DateTime::Span
        - Perl5 version uses Set::Infinite

=cut

method from_datetimes ($class: %param ) {
    return $class.new( %param );
}

method from_datetime_and_duration ($class: %param ) {
    my %tmp;
    # TODO - normalize duration syntaxes, such as 'hours => 12'
    my $duration = %param<duration>;
    if defined %param<start> 
    {
        %tmp<start> = %param<start>; 
        %tmp<end> = %param<start> + $duration;
    }
    else
    { 
        %tmp<after> = %param<after>; 
        %tmp<end> = %param<after> + $duration;
    }
    return $class.new( %tmp );
}

method set_time_zone( $tz ) {
    ...
}

method duration returns Object ($self: ) {
    return $self.size;
}

method start () returns Object {
    # TODO - change Inf to DateTime::Infinite
    return $.start;
}
method end () returns Object {
    # TODO - change Inf to DateTime::Infinite
    return $.end;
}

=kwid

= NAME

DateTime::Span - An object representing a datetime span

= SYNOPSIS

  use DateTime::Span;

  # XXX

= DESCRIPTION

This class represents a single span.

= CONSTRUCTORS

- `new()`

Without any parameters, returns an empty span.

- `new( start => $start )`

Given a start object, returns a span that has infinite size.

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
