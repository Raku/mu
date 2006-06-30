#!/usr/bin/pugs
use v6;

use Test;

plan( 6 );

use D; pass "(dummy instead of broken use_ok)";
skip( 1, q{is( D.meta.identifier.version, 0.1.0,
    'D is the correct version' );} );

use Relation; pass "(dummy instead of broken use_ok)";
skip( 1, q{is( Relation.meta.identifier.version, 0.1.0,
    'Relation is the correct version' );} );

use Relation::Example; pass "(dummy instead of broken use_ok)";
skip( 1, q{is( Relation::Example.meta.identifier.version, 0.1.0,
    'Relation::Example is the correct version' );} );
