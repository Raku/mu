use v6-alpha;

use Test;

plan( 2 );

use HTTP; pass "(dummy instead of broken use_ok)";
skip( 1, q{is( HTTP.WHO.version, 0.0.1,
    'HTTP is the correct version' );} );
