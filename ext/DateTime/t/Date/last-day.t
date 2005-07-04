use v6;
use Test;
plan 48;

use Date;
use t::Date::Data;

for 1..12 -> $m {
    my $date = Date.new( year => 2005, month => $m, day => 'last' );

    is( $date.month, $m, "month is $m" );
    is( $date.day, @*MonthLengths[ $m - 1 ], "day is @*MonthLengths[ $m - 1 ]" );
}

for 1..12 -> $m {
    my $date = Date.new( year => 2004, month => $m, day => 'last' );

    is( $date.month, $m, "month is $m" );
    is( $date.day, @*LeapYearMonthLengths[ $m - 1 ], "day is @*LeapYearMonthLengths[ $m - 1 ]" );
}
