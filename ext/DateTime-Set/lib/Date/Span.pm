use v6;

class Date::Span-0.01
    does Span;

# XXX - is this needed?
use Span;

=for TODO

    * change returned 'Inf' to 'Date::Infinite'

    * from_duration constructor needs some tweaks

    * tests

    * document differences from Perl5 DateTime::Span
        - Perl5 version uses Set::Infinite; this version uses Array
        - import Perl5 POD
        - this version can iterate()

    * set_time_zone

=cut

method from_dates ($class: %param is copy ) {
    %param<density> = Date::Duration.new( days => 1 ) unless defined %param<density>;
    return $class.new( %param );
}

method from_date_and_duration ($class: %param ) {
    my %tmp;
    %tmp<density> = %param<density>;
    %tmp<density> = Date::Duration.new( days => 1 ) unless defined %tmp<density>;
    # TODO - normalize duration syntaxes, such as 'days => 12'
    my $duration = %param<duration>;
    if defined %param<start> 
    {
        %tmp<start> = %param<start>; 
        %tmp<before> = %param<start> + $duration;
    }
    else
    { 
        %tmp<after> = %param<after>; 
        %tmp<before> = %param<after> + $duration;
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
    # TODO - change Inf to Date::Infinite
    return $.start;
}
method end () returns Object {
    # TODO - change Inf to Date::Infinite
    return $.end;
}

=kwid

= NAME

Date::Span - An object representing a date span

= SYNOPSIS

  use Date::Span;

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
