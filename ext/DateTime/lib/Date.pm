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
# this doesn't seem to work yet
#&now ::= &today;

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
