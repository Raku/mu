#!/usr/bin/perl

# based on the DateTime 0.29 test script of the same name.

use Test;

plan 6 * 12;

use Date::Gregorian;

my @last = ( 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 );
my @leap_last = @last »+« (0,1);

for 1 .. 12 -> $month {
    my $dt = date( :year(2001), :month($month), :day(-1) );

    is( $dt.year, 2001, 'check year' );
    is( $dt.month, $month, 'check month' );
    is( $dt.day, @last[ $month - 1 ], 'check day', :todo<bug>,
        :depends<oo/attributes/mutators.t> );
}

for 1 .. 12 -> $month {
    my $dt = date( :year(2004), :month($month), :day(-1) );

    is( $dt.year, 2004, 'check year' );
    is( $dt.month, $month, 'check month' );
    is( $dt.day, @leap_last[ $month - 1 ], 'check day', :todo<bug>,
        :depends<oo/attributes/mutators.t>
      );
}

