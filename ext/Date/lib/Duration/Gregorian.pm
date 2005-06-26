
class Duration::Gregorian is Duration;

use Date::Format::ISO8601;

our $iso8601_duration_re_anchored = rx:perl5/^$iso8601_duration_re$/;

has Int $.year;
has Int $.month;
has Int $.day;
has Int $.hour;
has Int $.minute;
has $.second;

# do subs that get declared first "win" the MMD war?
multi sub duration( Int|Real ?$seconds ) returns Duration::Gregorian is export {
    duration(:second($seconds));
}

multi sub duration( Int ?$year, Int ?$month, Int ?$day,
		    Int ?$hour, Int ?$minute, Int|Real ?$second #)
		     )
    returns Duration::Gregorian is export {
    Duration::Gregorian.new( :year($year), :month($month), :day($day),
			     :hour($hour), :minute($minute), :second($second),
			   );
}

# convert from an iso duration
multi method duration( Str $iso8601 ) returns Duration::Gregorian is export {

    # FIXME - missing XXXX 
    $iso8601 ~~ $iso8601_duration_re_anchored
        or die "can't match '$iso8601' to available ISO-8601 duration formats";

=pod

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

=cut


    return $duration;
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
