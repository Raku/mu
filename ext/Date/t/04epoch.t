#
#  from DateTime 0.29
#
# was called epoch, but epochs are relative.  this is a time_t.

use v6;

use Test;
plan 32

use Date::Gregorian;
{
    # Tests creating objects from time_t time
    my $t1 = date(:time_t(0));
    is( $t1.time_t, 0, "time_t should be 0" );

    is( $t1.second, 0, "seconds are correct on time_t 0" );
    is( $t1.minute, 0, "minutes are correct on time_t 0" );
    is( $t1.hour, 0, "hours are correct on time_t 0" );
    is( $t1.day, 1, "days are correct on time_t 0" );
    is( $t1.month, 1, "months are correct on time_t 0" );
    is( $t1.year, 1970, "year is correct on time_t 0" );
}

{
    my $dt = date(:time_t(3600));
    is( $dt.time_t, 3600, 'creation test from time_t = 3600 (compare to time_t)');
}

{
    # these tests could break if the time changed during the next three lines
    my $now = time;
    my $nowtest = date(now);
    my $nowtest2 = date(int(time));
    is( $nowtest.hour, $nowtest2.hour, "Hour: Create without args" );
    is( $nowtest.month, $nowtest2.month, "Month : Create without args" );
    is( $nowtest.minute, $nowtest2.minute, "Minute: Create without args" );
}

{
    my $time_ttest = date( :time_t('997121000'));

    is( $time_ttest.time_t, 997121000,
        "time_t method returns correct value");
    is( $time_ttest.hour, 18, "hour" );
    is( $time_ttest.min, 3, "minute" );
}

{
    my $dt = date( :time_t(3600));
    $dt.time_zone = '+0100';

    is( $dt.time_t, 3600, 'time_t is 3600' );
    is( $dt.hour, 2, 'hour is 2' );
}

{
    my $dt = date( :year(2000),
                            :month(1),
                            :day(1),
                            :hour(0),
                            :tz('-0100'),
                          );

    is( $dt.time_t, 3600, 'time_t is 3600' );
}

{
    my $dt = date( :time_t(0), :tz('-0100'), );

    is( $dt.offset, -3600, 'offset should be -3600' );
    is( $dt.time_t, 0, 'time_t is 0' );
}

# Adding/subtracting should affect time_t
{
    my $expected = '1049160602';
    my $time_ttest = date( :time_t($expected));

    is( $time_ttest.time_t, $expected,
        "time_t method returns correct value ($expected)");
    is( $time_ttest.hour, 1, "hour" );
    is( $time_ttest.min, 30, "minute" );

    $time_ttest.add( duration(:hours(2)));
    $expected += 2 * 60 * 60;

    is( $time_ttest.hour, 3, "adjusted hour" );
    is( $time_ttest.time_t, $expected,
        "time_t method returns correct adjusted value ($expected)");

}

#my $negative_time_t_ok = defined( (localtime(-1))[0] ) ? 1 : 0;

#SKIP:
{
    #skip 'Negative time_t times do not work on some operating systems, including Win32', 1
        #unless $negative_time_t_ok;

    is( date( :year(1904)).time_t, -2082844800,
        "time_t should work back to at least 1904" );
}

#SKIP:
{
    #skip 'Negative time_t times do not work on some operating systems, including Win32', 3
        #unless $negative_time_t_ok;

    my $dt = date( :time_t(-2082844800));
    is( $dt.year, 1904, 'year should be 1904' );
    is( $dt.month,   1, 'month should be 1904' );
    is( $dt.day,     1, 'day should be 1904' );
}

# time_t's can no longer be fractional.

{
    my $dt = date( :epoch(0.5));
    is( $dt.nanosecond, 500_000_000, 'nanosecond should be 500,000,000 with 0.5 as time_t' );

    isnt( $dt.time_t, 0, 'time_t should NOT be 0' );
    is( $dt.epoch, 0.5, 'hires_time_t should be 0.5' );
}

{
    my $dt = date( :time_t(0.1234567891));
    is( $dt.nanosecond, 123_456_789, 'nanosecond should be an integer ' );
}
