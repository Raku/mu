#!/usr/bin/pugs
#
# Please remember to update examples/output/vmethods/time if you change this
# file so its output will change.
#
# based on Ruby's ActiveSupport 1.0.4
#

use v6;

multi sub seconds (Int $value:) {
    $value;
}

our &second:=&second;

multi sub minutes (Int $value:) {
    $value * 60;
}

our &minute:=&minutes;

multi sub hours (Int $value:) {
    $value * 60.minutes;
}

our &hour:=&hours;

multi sub days (Int $value:) {
    $value * 24.hours;
}

our &day:=&days;

multi sub weeks (Int $value:) {
    $value * 7.days;
}

our &week:=&weeks;

multi sub fortnights (Int $value:) {
    $value * 2.weeks;
}

our &fortnight:=&fortnights;

multi sub months (Int $value:) {
    $value * 30.days;
}

our &month:=&months;

multi sub years (Int $value:) {
    $value * 365.days;
}

our &year:=&years;

multi sub centuries (Int $value:) {
    $value * 100.years;
}

our &century:=&centuries;

multi sub ago (Int $value: Int +$time = time()) {
    $time - $value;
}

our &until:=&ago;

multi sub since (Int $value: Int +$time = time()) {
    $time + $value;
}

our &from_now:=&since;

say "2 days is " ~ 2.days ~ " seconds.";
say "4 weeks is " ~ 4.weeks ~ " seconds.";
say "1 month is " ~ 1.month ~ " seconds.";

# say 2.days.ago
# say 3.weeks.from_now
# $1_week_ago = 1.week.ago;
# say 1.day.until($1_week_ago);
# say 2.months.since($1_week_ago);
