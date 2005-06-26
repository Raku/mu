#!/usr/bin/perl -w

# based on t/01sanity.t from DateTime 0.29

# note: this test does not test creating a date with a Time Zone

use Test;
use Date::Gregorian;

plan 21;

my $dg = Date::Gregorian.new( :year(1870), :month(10), :day(21),
			      :hour(12),   :minute(10),
			      :second(45.000123456),
			    );

is( $dg.year, '1870', "Year accessor, outside of the epoch" );
is( $dg.month, '10',  "Month accessor, outside the epoch" );
is( $dg.day, '21',    "Day accessor, outside the epoch" );
is( $dg.hour, '12',   "Hour accessor, outside the epoch" );
is( $dg.minute, '10', "Minute accessor, outside the epoch" );
is( $dg.second, '45.000123456', "Second accessor, outside the epoch" );
is( $dg.nanosecond, '123456', "nanosecond accessor, outside the epoch" );

my $dt = $dg.clone;

is( $dt.year, '1870', "Year should be identical" );
is( $dt.month, '10',  "Month should be identical" );
is( $dt.day, '21',    "Day should be identical" );
is( $dt.hour, '12',   "Hour should be identical" );
is( $dt.minute, '10', "Minute should be identical" );
is( $dt.second, '45.000123456', "Second should be identical" );
is( $dt.nanosecond, '123456', "nanosecond should be identical" );

ok( !($dt =:= $dg), "clone created different objects" );

# new tests for the Date::Gregorian API

$dg = date( 2000, 1, 1, 12, 12, 12 );

is($dg.year, 2000, "positional date()");
is($dg.month, 1, "positional date()");
is($dg.day, 1, "positional date()");
is($dg.hour, 12, "positional date()");
is($dg.minute, 12, "positional date()");
is($dg.second, 12, "positional date()");

#$dg = date( "2000" );
#is($dg.year, 2000, "iso8660 constructor");

#$dg = date( "200001" );
#is($dg.month, 1, "iso8660 constructor -");
