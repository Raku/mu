
class Date::Gregorian;

use Time::Zone;
use Duration::Gregorian;

does Date;

has Int $.year;
has Int $.month;
has Int $.day;
has Int $.hour;
has Int $.minute;
has Int $.second;
has Real $.subsecond;

has Time::Zone $.tz;

our $iso8601_re = rx:perl5:ix/\s*   # ** s/  # (go cperl-mode ;))
        # year
	(?: (\d{2}|\d{4}) -? | -)
        # month and day
        (?: (?: (\d\d) -? | - ) (\d\d) | (\d{3}) | [wW](\d{2}) )
        # optional time delimeter
	T \s*
	# time - optional, defaults to 0:00
        (\d\d):?(\d\d):?(\d\d)(?:\.(\d*))? \s*
	# time zone - optional, defaults to local time
	( Z | [+-](\d\d):?(\d\d)? )?
	/;      #/x  # (go cperl-mode ;))

our $iso8601_re_anchored = rx:perl5/^$iso8601_re$/;

# the below two constructors might look pointless and repetitive, and
# hopefully there will be a nice way of labelling this intent
multi method date( *@parts ) returns Date::Gregorian is export {
    Date::Gregorian.new( :year  (@parts[0]),
			 :month (@parts[1]),
			 :day   (@parts[2]),
			 :hour  (@parts[3]),
			 :minute(@parts[4]),
			 :second(@parts[5]),
			 :tz    (@parts[6])  );
}

multi method date( Int ?$year, Int ?$month, Int ?$day,
		   Int ?$hour, Int ?$minute, Int ?$second,
		   Time::Zone ?$tz )#?
    returns Date::Gregorian {
    Date::Gregorian.new( :year($year), :month($month), :day($day),
			 :hour($hour), :minute($minute), :second($second),
			 :tz($tz) );
}

# convert from an iso date
multi method date( Str $iso8601 ) returns Date::Gregorian is export {

    $iso8601 ~~ $iso8601_re_anchored
        or die "can't match '$iso8601' to available ISO-8601 formats";

    my $year = $0;
    if ( $year.defined and $year.chars == 2 ) {
	$year = 2000 + $year;  # XXX - fixme ;)
    }

    if ( $3 ) {
	# day of year ..
	!!!
    }
    elsif ( $4 ) {
	# week of year ..
	!!!
    }
    my $subsecond;
    if ( $8.defined ) {
	$subsecond = "0.$8" + 0;
    }

    my $tz;
    if ( $9.defined ) {
	$tz = tz $9;
    }

    return Date::Gregorian.new
	( :year($year), :month($1),  :day($2),
	  :hour($5),    :minute($6), :second($7),
	  :subsecond($subsecond),
	  :tz($tz) );
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
