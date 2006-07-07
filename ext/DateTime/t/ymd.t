use v6;
use Test;
plan 48;

use Date;

for [ 2004,  1,  1 ],
    [ 2004,  3, 31 ],
    [ 2004,  4,  1 ],
    [ 2004,  6, 30 ],
    [ 2004,  7,  1 ],
    [ 2004,  9, 30 ],
    [ 2004, 10,  1 ],
    [ 2004, 12, 31 ] {
    my ( $y, $m, $d ) = @$_;

    my $date = Date.new( year => $y, month => $m, day => $d );

    is( $date.ymd(), "$y-$m-$d", "ymd() is $y-$m-$d" );
    is( $date.ymd('!'), "$y!$m!$d", "ymd() is $y!$m!$d" );

    is( $date.mdy(), "$m-$d-$y", "mdy() is $m-$d-$y" );
    is( $date.mdy('^'), "$m^$d^$y", "mdy() is $m^$d^$y" );

    is( $date.dmy(), "$d-$m-$y", "dmy() is $d-$m-$y" );
    is( $date.dmy('#'), "$d#$m#$y", "dmy() is $d#$m#$y" );
}
