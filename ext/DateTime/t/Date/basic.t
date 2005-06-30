use v6;
use Test;

use Date;

my Date $date = Date.new();
isa_ok( $date, 'Date' );
