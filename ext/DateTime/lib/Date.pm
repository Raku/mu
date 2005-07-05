class Date;

#my subtype Month of Int where { 1 <= $^a <= 12 };
#my subtype Day of Int where { 1 <= $^a <= 31 };

has Int $.year;
has Int $.month;
has Int $.day; # where {  }; XXX - check that day is valid for month & year

my @MonthLengths =
    ( 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 );

my @LeapYearMonthLengths = @MonthLengths;
@LeapYearMonthLengths[1]++;

my @PreviousMonthDoY =
    ( 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334 );

my @PreviousMonthDoLY =
    ( 0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335 );

# has DateTime::Locale $.locale


# probably this should be done as "multi submethod BUILD", but that
# makes pugs barf last I checked
multi submethod BUILD () returns Date {
    return $_.today();
}

multi submethod BUILD (: Int|Real +$epoch) returns Date {
    $epoch = int $epoch;

    # waiting for localtime in Pugs
    my ( $y, $m, $d ) = (localtime)[5,4,3];
    $y += 1900;
    $m++;

    return $_.SUPER::new( year  => $y,
                          month => $m,
                          day   => $d,
                        );
}

multi submethod BUILD (: Str $string) returns Date {
    my $meth;
#    if ( $string ~~ /[today|now] { $meth = 'today }
#                     tomorrow    { $meth = 'tomorrow' }
#                     yesterday   { $meth = 'yesterday' }
#                    / ) {
#        return $_.$meth();
#    }
#    else {
#        # load a heavier weight parser - hand waving ensues
#    }
}

# day as Str where { rx:i/^last$/ }
multi submethod BUILD (: Int +$year, Int +$month = 1, Int|Str +$day is copy = 1) returns Date {
    if $day ~~ rx:perl5<i>/^last$/ {
        my @lengths := _is_leap_year($year) ?? @LeapYearMonthLengths :: @MonthLengths;

        $day = @lengths[ $month - 1 ];
    }

    $.year  = $year;
    $.month = $month;
    $.day   = $day;
}

method today () returns Date {
    return $_.new( epoch => time );
}

our &Date::now ::= &Date::today;

method tomorrow () returns Date {
    return $_.new( epoch => time ).add( :days => 1 );
}

method yesterday () returns Date {
    return $_.new( epoch => time ).subtract( :days => 1 );
}

sub _is_leap_year (Int $year) returns bool {
    if $year % 400 == 0 {
        return bool::true;
    }
    elsif $year % 100 == 0 {
        return bool::false;
    }
    elsif $year % 4 == 0 {
        return bool::true;
    }

    return bool::false;
}

method quarter () returns Int {
    return int( ( 1.0 / 3.1 ) * $.month ) + 1;
}

method day_of_quarter () returns Int {
    my $doy = $_.day_of_year();

    my @doy := _is_leap_year($.year) ?? @PreviousMonthDoLY :: @PreviousMonthDoY;

    return $doy - @doy[ ( 3 * $_.quarter() ) - 3 ];
}

method day_of_year () returns Int {
    my @doy := _is_leap_year($.year) ?? @PreviousMonthDoLY :: @PreviousMonthDoY;

    @doy[$.month - 1] + $.day;
}

method ymd (Str ?$sep = "-") returns Str {
    return [$.year, $.month, $.day].join($sep);
}

method mdy (Str ?$sep = "-") returns Str {
    return [$.month, $.day, $.year].join($sep);
}

method dmy (Str ?$sep = "-") returns Str {
    return [$.day, $.month, $.year].join($sep);
}
