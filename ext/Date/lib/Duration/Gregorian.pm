
class Duration::Gregorian;

use Date::Gregorian;

does Duration;

has Int $.year;
has Int $.month;
has Int $.day;
has Int $.hour;
has Int $.minute;
has Int $.second;
has Real $.subsecond;

multi method duration( *@parts ) returns Duration::Gregorian is export {
    Duration::Gregorian.new( :year     ($parts[0]),
			     :month    ($parts[1]),
			     :day      ($parts[2]),
			     :hour     ($parts[3]),
			     :minute   ($parts[4]),
			     :second   ($parts[5]),
			     :subsecond($parts[6])  );
}

multi method duration( Int ?$year, Int ?$month, Int ?$day,
		       Int ?$hour, Int ?$minute, Int ?$second,
		       Real ?$subsecond )
    returns Date::Gregorian is export {
    Date::Gregorian.new( :year($year), :month($month), :day($day),
			 :hour($hour), :minute($minute), :second($second),
			 :subsecond($subsecond) );
}

our $iso8601_dur_re = rx:perl5:ix/\s*   # ** s/  # (go cperl-mode ;))
        P \s*
	(?: (\d+) \s* Y )? \s*
	(?: (\d+) \s* M )? \s*
	(?: (\d+) \s* D )? \s*
        (?: T \s*
	    (?: (\d+) \s* H )? \s*
	    (?: (\d+) \s* M )? \s*
	    (?: (\d+) (?: \.(\d+))? \s* S )? \s* )?
	/;      #/x  # (go cperl-mode ;))

our $iso8601_duration_re = rx:perl5:ix/\s*   # ** s/  # (go cperl-mode ;))
       # below is a nice demonstration of why we need rules :)
       (?:  ( # $0
              $iso8601_dur_re ( # $8
                                \/ $iso8601_re )? )
       |    ( # $18
              $iso8601_re \/ (?: ( # $28
                                 $iso8601_dur_re ) |
                                 ( # $36
				 $iso8601_re ) ) )
	)/;      #/x  # (go cperl-mode ;))

our $iso8601_duration_re_anchored = rx:perl5/^$iso8601_duration_re$/;

# convert from an iso duration
multi method duration( Str $iso8601 ) returns Date::Gregorian is export {

    # FIXME - missing XXXX 
    $iso8601 ~~ $iso8601_duration_re_anchored
        or die "can't match '$iso8601' to available ISO-8601 duration formats";

    my $duration;
    if ( defined $0 ) {
	$duration = duration($/[1..7]);
	if ( defined $8 ) {
	    $duration.end = duration($/[9..17]);
	}
    }
    elsif ( defined $/[18] ) {
	my $start = date($/[19..27]);

	if ( defined $/[28] ) {
	    $duration = duration($/[29..35]);
	    $duration.start = $start;
	} else {
	    my $end = date($/[37..35]);
	    $duration = Duration::Gregorian.new
		( :start($start), :end($end) );
	}
    } else {
	!!!
    }

    return $duration;
}

# convert from a unix time_t
multi method date( Int $time_t ) returns Date::Gregorian is export {
    Date.Gregorian.new( :year(1970), :month(1),  :day(1),
			:hour(0),    :minute(0), :second(0),
			:tz(tz "UTC") ) + $time_t;
}

# convert from a perl float
multi method date( Real $epoch ) returns Date::Gregorian is export {
    Date.Gregorian.new( :year(1970), :month(1),  :day(1),
			:hour(0),    :minute(0), :second(0),
			:tz(tz "UTC") ) + $epoch;
}

# operations with Duration::Gregorian constructors..
multi method infix:<+>( $self: Str|Int|Real $iso8601_dur ) {
    $self + duration($iso8601_dur);
}

multi method infix:<->( $self: Int|Real $iso8601_dur ) {
    $self + duration($iso8601_dur);
}

multi method infix:<->( $self: Str $what ) {
    my $other = eval { date($what) };
    if ( $other ) {
	return $self - $other;
    } else {
	return $self - duration($what);
    }
}
