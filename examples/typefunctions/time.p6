
#
# based on Ruby's ActiveSupport 1.0.4
#

sub seconds {
    $_;
}
our &second:=&second;

sub minutes {
    $_ * 60;
}
our &minute:=&minutes;

sub hours {
    $_ * 60.minutes;
}
our &hour:=&hours;

sub days {
    $_ * 24.hours;
}
our &day:=&days;

sub weeks {
    $_ * 7.days;
}
our &week:=&weeks;

sub fortnights {
    $_ * 2.weeks;
}
our &fortnight:=&fortnights;

sub months {
    $_ * 30.days;
}
our &month:=&month;

sub years {
    $_ * 365.days;
}
our &year:=&years;

sub centuries {
    $_ * 100.years;
}
our &century:=&centuries;

sub ago($value: +$time = time()) {
    $time - $value;
}
our &until:=&ago;

sub since($value: +$time = time()) {
    $time + $value;
}
our &from_now:=&since;
