use v6-alpha;

use Test;

plan( 2 );

use Set::Relation; pass "(dummy instead of broken use_ok)";
skip( 1, q{is( Set::Relation.meta.identifier.version, 0.1.0,
    'Set::Relation is the correct version' );} );
