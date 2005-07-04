use v6;
use Test;
plan 8;

use Date;

for [ 2004,  1,  1, 1 ],
    [ 2004,  3, 31, 1 ],
    [ 2004,  4,  1, 2 ],
    [ 2004,  6, 30, 2 ],
    [ 2004,  7,  1, 3 ],
    [ 2004,  9, 30, 3 ],
    [ 2004, 10,  1, 4 ],
    [ 2004, 12, 31, 4 ] {
    my ( $y, $m, $d, $q ) = @$_;

    my $date = Date.new( year => $y, month => $m, day => $d );

    is( $date.quarter, $q, "quarter is $q" );
}
