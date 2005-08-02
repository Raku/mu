use v6;

# Set::Symbols is defined in the Recurrence.pm package
use Set::Symbols;

class Span-0.01
    does Set::Symbols
{

    use Span::Num;
    use Span::Int;
    use Span::Code;

    has $.span;

=for TODO
    
    * test the integration with Span::Code objects

Known bugs:
  
    * 'undefine span' is wrong - use empty_span instead (in set_end() / set_start() )

    * _normalize_parameter - must check the internal span type
    
    * all objects should be cloned before inserting in the span
    
Ideas:

    * besides :int / :density(), there could be :next(&coderef) / :previous(&coderef)
    
    * from_start_and_duration
    
    * set_start_open / set_start_closed
    * set_end_open / set_end_closed
        - better names ?

    * set_density
    
    * as_list
        - how to create a lazy list ?
    
    * empty_span
        - test with "density"
    * universal_span

    * create a store for single elements (Span::Singleton)
    * is_singleton
    
From "Set" API:

    * equal/not_equal
    * symmetric_difference
    * proper_subset
    * proper_superset
    * subset
    * superset
    * includes/member/has

=cut

submethod BUILD ($class: *%param is copy ) {
    # TODO - error message on ignored parameters
    my ( $start, $end );
    my bool $start_is_open;
    my bool $end_is_open;
    my $density;
    $density = 1 if %param<int>;
    $density = %param<density> if defined %param<density>;

    %param<object> = %param<span> if defined( %param<span> );

    if defined( %param<object> ) 
    {
        if %param<object>.isa( $class.ref )
        {
            $.span = %param<object>.span;
            return;
        }
        if %param<object>.isa( 'Span::Num' | 'Span::Int' | 'Span::Code' )
        {
            $.span = %param<object>;
            return;
        }
        if %param<object>.ref eq 'Array' {
            if %param<object>.elems > 0
            {
                %param<start> = %param<object>[0];
                %param<end> = %param<object>[-1];
            }
        }
        else
        {
            %param<start> = %param<end> = %param<object>;
        }
    }

    if ( defined $density )
    {
        if defined( %param<start> )  { $start = %param<start> };
        if defined( %param<after> )  { $start = %param<after> + $density };
        if defined( %param<end> )    { $end =   %param<end> };
        if defined( %param<before> ) { $end =   %param<before> - $density };

        if   !defined( $start ) &&  defined( $end ) { $start = -Inf }
        elsif defined( $start ) && !defined( $end ) { $end = Inf }

        die "start must be less or equal to end" if $start > $end;

        if defined( $start ) && defined( $end ) {
            $.span = Span::Int.new( :start($start), 
                                    :end($end), 
                                    :density($density) );
        }
        else
        {
            $.span = Span::Int.empty_span( :density($density) );
        }
    }
    else
    {
        if defined( %param<start> )  { $start = %param<start>;  $start_is_open = bool::false };
        if defined( %param<after> )  { $start = %param<after>;  $start_is_open = bool::true };
        if defined( %param<end> )    { $end =   %param<end>;    $end_is_open =   bool::false };
        if defined( %param<before> ) { $end =   %param<before>; $end_is_open =   bool::true };

        if   !defined( $start ) &&  defined( $end ) { $start = -Inf }
        elsif defined( $start ) && !defined( $end ) { $end = Inf }

        $start_is_open = bool::true if $start == -Inf;
        $end_is_open =   bool::true if $end == Inf;

        die "start must be less or equal to end" if $start > $end;

        if defined( $start ) && defined( $end ) {
            $.span = Span::Num.new( :start($start), 
                                    :end($end), 
                                    :start_is_open($start_is_open), 
                                    :end_is_open($end_is_open) );
        }
        else
        {
            $.span = Span::Num.empty_span();
        }
    }
}

method is_empty () returns bool { return $.span.is_empty }

method is_infinite ($self: ) returns bool { 
    return $self.start == -Inf || $self.end == Inf
}

method start () returns Object { return $.span.start }

method set_start ($self: $start ) {
    if $.span.is_empty 
    {
        $.span = $self.new( start => $start ).span;
    }
    else
    {
        my int $cmp = $start <=> $.span.end;
        if $cmp > 0 {
            warn "setting start bigger than end yields an empty set";
            undefine $.span;
        }
        elsif $cmp == 0 && ( $.span.start_is_open || $.span.end_is_open ) {
            warn "setting start equal to end yields an empty set";
            undefine $.span;
        }
        else
        {
            $.span = $.span.clone;
            $.span.start = $start;
        }
    }
}

method end () returns Object { return $.span.end }

method set_end ($self: Object $end ) {
    if $.span.is_empty
    {
        $.span = $self.new( end => $end ).span;
    }
    else
    {
        my int $cmp = $.span.start <=> $end;
        if $cmp > 0 {
            warn "setting start bigger than end yields an empty set";
            undefine $.span;
        }
        elsif $cmp == 0 && ( $.span.start_is_open || $.span.end_is_open ) {
            warn "setting start equal to end yields an empty set";
            undefine $.span;
        }
        else
        {
            $.span = $.span.clone;
            $.span.end = $end;
        }
    }
}

method start_is_open () returns Bool {
    return bool::false if $.span.is_empty;
    return $.span.start_is_open;
}
method start_is_closed () returns Bool {
    return bool::false if $.span.is_empty;
    return $.span.start_is_closed;
}
method end_is_open () returns Bool {
    return bool::false if $.span.is_empty;
    return $.span.end_is_open;
}
method end_is_closed () returns Bool {
    return bool::false if $.span.is_empty;
    return $.span.end_is_closed;
}

# Removed - this is too specific - it only apply to Span::Int objects
# method density () returns Object {
#    return $.span.density;
# }

method stringify () returns String {
    return $.span.stringify;
}

method size () returns Object {
    return undef if $.span.is_empty;
    return $.span.size;
}

sub _normalize_parameter ( $self, $param ) {
    # TODO - reorder these rules or move to the subclasses
    my $span0 = $self.isa( 'Span' ) ?? $self.span :: $self;
    my $span1 = $param;
    # say "normalize ", $span0, $span1;
    if $span1.isa( 'Recurrence' ) {
        my $result = Span::Num.new( start => -Inf, end => Inf );
        return $span0, $self.new( span => Span::Code.new( recurrence => $param, span => $result ) );
    }
    $span1 = $span1.span if $span1.isa( 'Span' );
    return $span0, $span1 if $span1.isa( 'Span::Code' | $span0.ref );
    if $span1.isa( 'Span::Num' ) 
    {
        # say "span-num";
        return $span0, $span1 if $span0.isa( 'Span::Code' );
        return $span0, $span0.new( :density($span0.density) ) if $span1.is_empty;
        # $span is a span, but it has a different type than $self
        my $start = $span1.start;
        my $end =   $span1.end;
        $start += $span0.density if $start != -Inf && $span1.start_is_open;
        $end   -= $span0.density if $end   !=  Inf && $span1.end_is_open;
        my $ret = $span0.new( :start($start), :end($end), :density($span0.density) );
        # say "returning ", $span0.stringify, ",", $ret.stringify;
        return $span0, $ret;
    }
    if $span1.isa( 'Span::Int' ) 
    {
        # say $span1, $span0;
        #...
        ($span1, $span0) = _normalize_parameter( $span1, $span0 );
        # say "returned ", $span0, ",", $span1;
        return $span0, $span1;
        # XXX - this doesn't work
        # return @a.reverse;
    }
    # $span is some kind of scalar
    return $span0, $span0.new( start => $span1, end => $span1 );
}

method compare ($self: $span is copy) returns int { 
    my ($span0, $span1) = _normalize_parameter( $self, $span );
    return 0  if $span0.is_empty && $span1.is_empty;
    return -1 if $span0.is_empty;
    return 1  if $span1.is_empty;
    return $span0.compare( $span1 );
}

method contains ($self: $span is copy) returns bool {
    return bool::false if $.span.is_empty;
    
    my ($span0, $span1) = _normalize_parameter( $self, $span );
    my @union = $span0.union( $span1 );
    
    # XXX this should work
    # my @union = $self.span.union( $span.span );
    
    return bool::false if @union.elems == 2;
    return @union[0].compare( $self.span ) == 0;
}

method intersects ($self: $span is copy) returns bool {
    return bool::false if $.span.is_empty;
    
    my ($span0, $span1) = _normalize_parameter( $self, $span );
    my @union = $span0.union( $span1 );
    
    # XXX - this should work
    # my @union = $self.span.union( $span.span );

    return @union.elems == 1;
}

method union ($self: $span is copy) returns List of Span { 
    my ($span0, $span1) = _normalize_parameter( $self, $span );
    return $self.new( span => $span1 ) if $span0.is_empty;
    return $self.new( span => $span0 ) if $span1.is_empty;
    my @union = $span0.union( $span1 );
    return @union.map:{ $self.new( span => $_ ) };
}

method intersection ($self: $span is copy) returns List of Span {
    # return $self.clone if $self.is_empty;
    my ($span0, $span1) = _normalize_parameter( $self, $span );
    return $self.new( span => $span1 ) if $span1.is_empty;
    return $self.new( span => $span0 ) if $span0.is_empty;
    my @span = $span0.intersection( $span1 );
    return @span.map:{ $self.new( span => $_ ) };
}

method complement ($self: ) returns List of Span {
    my $span0 = $self.span;
    my @span0 = $span0.complement;
    return @span0.map:{ $self.new( span => $_ ) };
}

method difference ($self: $span is copy) returns List of Span {
    return $self.clone if $self.is_empty;

    my ($span0, $span1) = _normalize_parameter( $self, $span );

    # XXX - why this doesn't work?
    # say 'diff ' , $span0.stringify, ' to ', $span1.stringify;
    # my @span0 = $span0.difference( $span1.span );
    # return @span0.map:{ $self.new( span => $_ ) };

    my @span1 = $span1.complement;
    # say $_.stringify for @span1;
    @span1 = @span1.map:{ $self.intersection( $_ ) };
    return @span1;
}

method next ( $x ) { 
    return $.span.next( $x );
}

method previous ( $x ) { 
    return $.span.previous( $x );
}

method current ( $x ) {
    return $.span.next( $.span.previous( $x ) );
}

method closest ($self: $x ) {
    my $n = $self.next( $x );
    my $p = $self.current( $x );
    return $n - $x < $x - $p ?? $n :: $p;
}

method iterator ($self: ) returns Span::Iterator {
    if ( ! defined( $.span.density ) &&
         ! $.span.isa( 'Span::Code' ) )
    {
        warn "creating an iterator for a continuous Span";
    }
    return ::Span::Iterator.new( span => $.span );
}

coro lazy ($self: ) {
    my $iter = $self.iterator();
    loop { 
        my $n = $iter.next;
        return unless defined $n;
        yield $n;
    }
}

} # class Span

class Span::Iterator
{
    has $.current;
    has $.span;
    submethod BUILD ( $.span ) {}
    method next () {
        if defined $.current {
            $.current = $.span.next( $.current )
        }
        else {
            $.current = $.span.next( -Inf )
        }
        undefine $.current if $.current == Inf;
        return $.current;
    }
    method previous () {
        if defined $.current {
            $.current = $.span.previous( $.current )
        }
        else {
            $.current = $.span.previous( Inf )
        }
        undefine $.current if $.current == -Inf;
        return $.current;
    }
    method reset () {
        undefine $.current;
    }
}

=kwid

= NAME

Span - An object representing a single span

= SYNOPSIS

    use Span;

    $int_span = Span.new( :int, start => 0, end => 10 );
    #  [10,20]   11 things
    
    $one_day = Date::Duration.new( days => 1 );
    $date_span = Span.new( :density($one_day), start => $dstart, end => $dend );
    # [2005-07-15,2005-07-20]    6 days

    $num_span = Span.new( start => -1.0, end => 2.5 );
    #  [-1.0,2.5]   
    
    $datetime_span = Span.new( start => $dtstart, before => $dtend );
    #  [2005-07-03T00:00:00.0,2005-07-04T00:00:00.0) 

    $time_span = Span.new( start => $tstart, end => $tend );
    #  [00:00:00.0,24:00:00.0]

= DESCRIPTION

This class represents a "span".

Depending on your data, you may need a "discrete" or a "continuous" span.
This class implements both types of spans.

- Discrete span

This is used to represent ranges of things that can be counted, such as beats per minute, 
wags per second, or holidays per month.

It is also used when the objects represent "chunks" of data, such as date objects without the time part.

Discrete spans take a "units" specifier, which can be either `:int` or `:density($size)`.

The unit specifier is used for several things. 
For example, when iterating through the span range, the unit is added to the current value to get the next value.
The unit specifier is also used internally when joining two spans, and when getting the span size.

Create a span of 10 up to 20:

    $int_span = Span.new( :int, start => 0, end => 10 );
    #  [10,20]   11 things
    
Create a span of dates:
    
    $one_day = Date::Duration.new( days => 1 );
    $date_span = Span.new( :density($one_day), start => $dstart, end => $dend );
    # [2005-07-15,2005-07-20]    6 days

When a Span object is intersected to a Recurrence object, the result is a discrete span:

    $month = Span.new( start => $dstart, end => $dend );
    my $month_holidays = $month.intersection( $recurrence_holidays );

- Continuous span

This type os span is used to represent continuous measurements, such as times, temperatures, or price ranges.

Create a span of real numbers:

    $num_span = Span.new( start => -1.0, end => 2.5 );
    #  [-1.0,2.5]   
    
Create a span representing a day with full precision:
    
    $datetime_span = Span.new( start => $dtstart, before => $dtend );
    #  [2005-07-03T00:00:00.0,2005-07-04T00:00:00.0) 

Create a span representing 0 to 24 hours with full precision:
    
    $time_span = Span.new( start => $tstart, end => $tend );
    #  [00:00:00.0,24:00:00.0]

It would not make sense to iterate through a continuous span, because the "separation" 
between values would be very close to zero:

    $iterator.next;   # 1.00000000000000
    $iterator.next;   # 1.00000000000001
    $iterator.next;   # 1.00000000000002

= CONSTRUCTORS

- `new( start => 1.0, end => 2.0 )`

Creates a span.

Dies if `start` if bigger than `end`.

- `new( after => 1.0, before => 2.0 )`

Creates an "open" span, that is, it does not include the start or end values.

- `new( start => 1.0 )`

Given a start value, creates a span that has infinite size.

- `new( :int, start => 1, end => 2 )`

Creates a span with "integer" semantics.

- `new( :density($day_duration), start => $first_day, end => $last_day )`

Creates a span with "day" semantics.

- `new( object => 1 )`

Creates a span with a single element. This is the same as `new( start => $object, end => $object )`.

- `new( span => $span )`

Creates a `Span` object using an existing span.

- `new()`

Without any parameters, creates an empty span.

= OBJECT METHODS

The following methods are available for Span objects:

- `start()` / `end()`

Return the start or end value of the span.

These methods may return nothing if the span is empty.

- `set_start( $object )` / `set_end( $object )`

Change the start or end value of the span.

These methods may raise a warning if the new value would put the span 
in an invalid state, such as `start` bigger than `end` (the span is
emptied in this case).

- `start_is_open()` / `end_is_open()` / `start_is_closed()` / `end_is_closed()`

Return a logical value, whether the `start` or `end` values belong to the span ("closed") or not ("open").

- `size()`

Return the "size" of the span.

For example: if `start` and `end` are times, then `size` will be a duration.

- `contains( $object )` / `intersects( $object )`

These methods return a logical value. 

The argument can be a Span object, a Recurrence object, or a scalar value.

- `union( $span )`

Returns a list of Spans. If the spans overlap, then the list contains a single
span. Otherwise, it contains two spans. Spans of recurrences may yield three spans.

The argument can be a Span object, a Recurrence object, or a scalar value.

- `complement()`

Returns a list of Spans. If one side of the span if Infinite, then the list contains
a single span. Otherwise, it contains two spans.

- `difference( $span )`

Returns a list of Spans. The list may have from zero to 2 spans.

The argument can be a Span object, a Recurrence object, or a scalar value.

- `intersection( $span )` 

Returns a list os Spans. The list may be empty, if the spans don't intersect. 
Otherwise, it contains a single span.

The argument can be a Span object, a Recurrence object, or a scalar value.

- `stringify ()`

Return a string representation of the span.

- `compare( $span )`

Compares the spans numerically. Returns -1, 0, or 1.

The argument can be a Span object, a Recurrence object, or a scalar value.

- `is_empty()`

Returns `true` if the span is empty.

- `is_infinite()`

Returns true if the start or the end of the span are Infinite.

- `iterator()`

Returns an iterator:

    $iter = $span.iterator;
    say $i while $i = $iter.next;

The iterator has `next()`, `previous()`, `current()`, and `reset()` methods.

If the span is "continuous", this method emits a warning and returns undef.

- `span()`

Returns the internal object that represents the Span.

- `lazy()`

Makes a lazy iterator:

    say $a while $a = $span.lazy;

= AUTHOR

Flavio S. Glock, <fglock@pucrs.br>

= COPYRIGHT

Copyright (c) 2005, Flavio S. Glock.  All rights reserved.

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut
