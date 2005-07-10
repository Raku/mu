use v6;
use Test;
plan 36;

use Date;

{
    # somewhat arbitrary number where month & day are not == 1
    my $epoch = 86400 * 40;
    my $t = localtime($epoch);

    my Date $date = Date.new( epoch => $epoch );
    isa_ok( $date, 'Date' );

    is( $t.year, $date.year, "year matches localtime()" );
    is( $t.month, $date.month, "month matches localtime()" );
    is( $t.day, $date.day, "day matches localtime()" );
}

{
    my $t = localtime();

    my Date $date = Date.new();
    isa_ok( $date, 'Date' );

    is( $t.year, $date.year, "year is today's year" );
    is( $t.month, $date.month, "month is today's month" );
    is( $t.day, $date.day, "day is today's day" );
}

{
    my $t = localtime();

    my Date $date = Date.now();
    isa_ok( $date, 'Date' );

    is( $t.year, $date.year, "year is today's year" );
    is( $t.month, $date.month, "month is today's month" );
    is( $t.day, $date.day, "day is today's day" );
}

{
    my $t = localtime();

    my Date $date = Date.new( 'now' );
    isa_ok( $date, 'Date' );

    is( $t.year, $date.year, "year is today's year" );
    is( $t.month, $date.month, "month is today's month" );
    is( $t.day, $date.day, "day is today's day" );
}

{
    my $t = localtime();

    my Date $date = Date.today();
    isa_ok( $date, 'Date' );

    is( $t.year, $date.year, "year is today's year" );
    is( $t.month, $date.month, "month is today's month" );
    is( $t.day, $date.day, "day is today's day" );
}

{
    my $t = localtime();

    my Date $date = Date.new( 'today' );
    isa_ok( $date, 'Date' );

    is( $t.year, $date.year, "year is today's year" );
    is( $t.month, $date.month, "month is today's month" );
    is( $t.day, $date.day, "day is today's day" );
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
