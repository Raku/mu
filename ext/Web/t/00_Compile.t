use v6-alpha;

use Test;

plan( 2 );

use Web; pass "(dummy instead of broken use_ok)";
skip( 1, q{is( Web.WHO.version, 0.0.1,
    'Web is the correct version' );} );
