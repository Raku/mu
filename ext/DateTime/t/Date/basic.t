use v6;
use Test;
plan 36;

use Date;

my ( $y, $m, $d ) = (localtime)[5,4,3];
$y += 1900;
$m++;

{
    my Date $date = Date.new( epoch => time );
    isa_ok( $date, 'Date' );

    is( $date.year, $y, "year is today's year" );
    is( $date.month, $m, "month is today's month" );
    is( $date.day, $d, "day is today's day" );
}

{
    my Date $date = Date.new();
    isa_ok( $date, 'Date' );

    is( $date.year, $y, "year is today's year" );
    is( $date.month, $m, "month is today's month" );
    is( $date.day, $d, "day is today's day" );
}

{
    my Date $date = Date.now();
    isa_ok( $date, 'Date' );

    is( $date.year, $y, "year is today's year" );
    is( $date.month, $m, "month is today's month" );
    is( $date.day, $d, "day is today's day" );
}

{
    my Date $date = Date.new( 'now' );
    isa_ok( $date, 'Date' );

    is( $date.year, $y, "year is today's year" );
    is( $date.month, $m, "month is today's month" );
    is( $date.day, $d, "day is today's day" );
}

{
    my Date $date = Date.today();
    isa_ok( $date, 'Date' );

    is( $date.year, $y, "year is today's year" );
    is( $date.month, $m, "month is today's month" );
    is( $date.day, $d, "day is today's day" );
}

{
    my Date $date = Date.new( 'today' );
    isa_ok( $date, 'Date' );

    is( $date.year, $y, "year is today's year" );
    is( $date.month, $m, "month is today's month" );
    is( $date.day, $d, "day is today's day" );
}

{
    my Date $date = Date.new( year => 2004 );
    isa_ok( $date, 'Date' );

    is( $date.year, 2004, "year is 2004" );
    is( $date.month, 1, "month is 1" );
    is( $date.day, 1, "day is 1" );
}

{
    my Date $date = Date.new( year => 2004, month => 6 );
    isa_ok( $date, 'Date' );

    is( $date.year, 2004, "year is 2004" );
    is( $date.month, 6, "month is 6" );
    is( $date.day, 1, "day is 1" );
}

{
    my Date $date = Date.new( year => 2004, month => 6, day => 29 );
    isa_ok( $date, 'Date' );

    is( $date.year, 2004, "year is 2004" );
    is( $date.month, 6, "month is 6" );
    is( $date.day, 29, "day is 29" );
}
