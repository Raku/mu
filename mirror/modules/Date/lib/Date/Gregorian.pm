
class Date::Gregorian is Date;

use Time::Zone;
use Date::Format::ISO8601;

has Int $.year;
has Int $.month;
has Int $.day;
has Int $.hour;
has Int $.minute;
has Real $.second;

has Time::Zone $.tz;

our $iso8601_re_anchored = rx:perl5/^$iso8601_re$/;

multi sub date( Int ?$year, Int ?$month, Int ?$day,
		   Int ?$hour, Int ?$minute, Int|Real ?$second,
		   Str|Time::Zone ?$tz )#?
    returns Date::Gregorian is export {
	#$tz = tz($tz) if $tz.defined and !$tz.isa("Time::Zone");
    return
	Date::Gregorian.new( :year($year), :month($month), :day($day),
			     :hour($hour), :minute($minute), :second($second),
			     :tz($tz) );
}

# convert from an iso date
multi sub date( Str $iso8601 ) returns Date::Gregorian is export {

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
    my $second = $7 || 0;
    if ( $8.defined ) {
	$second += "0.$8";
    }

    my $tz;
    if ( $9.defined ) {
	$tz = tz $9;
    }

    return Date::Gregorian.new
	( :year($year), :month($1),  :day($2),
	  :hour($5),    :minute($6), :second( $second ),
	  :tz($tz) );
}

# convert from a unix time_t
multi sub time_t( Int $time_t ) returns Date::Gregorian is export {
    Date::Gregorian.new( :year(1970), :month(1),  :day(1),
			:hour(0),    :minute(0), :second(0),
			:tz(tz "UTC") ) + $time_t;
}

# convert from a perl float
multi sub epoch( Real $epoch ) returns Date::Gregorian is export {
    Date::Gregorian.new( :year(1970), :month(1),  :day(1),
			:hour(0),    :minute(0), :second(0),
			:tz(tz "UTC") ) + $epoch;
}

method nanosecond returns Int {
    my $sec = $.second;
    my $round_sec = int($sec);
    my $rem = $sec - $round_sec;
    my $nano = $rem * 1e9;
    return int($nano);
    #int( ($.second - int($.second) ) * 1e9);
}
method microsecond returns Int {
    int( ($.second - int($.second) ) * 1e6);
}
method millisecond returns Int {
    int( ($.second - int($.second) ) * 1e3);
}
method ce_year returns Int { $.year }
method quarter returns Int { int(($.month+2)/3) }
method month_0 returns Int { $.month - 1 }
method yy returns Int { die if $.year < 0; $.year % 100 }
method century returns Int { die if $.year < 0; int($.year / 100) }

our @months = <January February March     April   May      June
               July    August   September October November December>;
method month_name returns Str { @months[./month_0] }

our @months_abbr = <Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec>;
method month_abbr returns Str { @months_abbr[./month_0] }

# yes, these functions are not "correct" :)
method doy returns Int { $.month * 30 + $.day }
#our &Date::Gregorian::day_of_year ::= &Date::Gregorian::doy;
method week returns Int { int (./doy / 7) }

method day_of_month returns Int { $.day }
method day_of_month_0 returns Int { $.day - 1 }
method day_0 returns Int { $.day - 1 }
method mday returns Int { $.day }
method mday_0 returns Int { $.day - 1 }

#use Duration::Gregorian;# qw(duration);

#BEGIN {
#our &Date::Gregorian::duration ::= &Duration::Gregorian::duration;
#}

# operations with Duration::Gregorian constructors..
#multi method infix:<+>( $self: Str|Int|Real $iso8601_dur ) {
    #$self + duration($iso8601_dur);
#}

# still an outstanding bug in overloaded -
#multi method infix:<->( $self: Int|Real $iso8601_dur ) {
    #$self + duration($iso8601_dur);
#}

#multi method infix:<->( $self: Str $what ) {
    #my $other = eval { date($what) };
    #if ( $other ) {
	#return $self - $other;
    #} else {
	#return $self - duration($what);
    #}
#}

