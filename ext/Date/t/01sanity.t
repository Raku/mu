#!/usr/bin/perl -w

# based on t/01sanity.t from DateTime 0.29

use Test;

#plan 15;

BEGIN { plan 15;
	use_ok("Date::Gregorian") };

{
    my $dg = Date::Gregorian.new( :year(1870), :month(10), :day(21),
                           	  :hour(12),   :minute(10),
				  :second(45.000123456),
				  :time_zone('UTC') );

    is( $dg.year, '1870', "Year accessor, outside of the epoch" );
    is( $dg.month, '10',  "Month accessor, outside the epoch" );
    is( $dg.day, '21',    "Day accessor, outside the epoch" );
    is( $dg.hour, '12',   "Hour accessor, outside the epoch" );
    is( $dg.minute, '10', "Minute accessor, outside the epoch" );
    is( $dg.second, '45', "Second accessor, outside the epoch" );
    is( $dg.nanosecond, '123456', "nanosecond accessor, outside the epoch" );

    $dt = $dg.new;

    is( $dt.year, '1870', "Year should be identical" );
    is( $dt.month, '10',  "Month should be identical" );
    is( $dt.day, '21',    "Day should be identical" );
    is( $dt.hour, '12',   "Hour should be identical" );
    is( $dt.minute, '10', "Minute should be identical" );
    is( $dt.second, '45', "Second should be identical" );
    is( $dt.nanosecond, '123456', "nanosecond should be identical" );

    ok( !($dt =:= $dg), "clone created different objects" );
}

