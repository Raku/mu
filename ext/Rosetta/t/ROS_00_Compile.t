#!/usr/bin/pugs
use v6;

use lib <t/lib ext/Rosetta/t/lib>;

use Test;

plan( 12 );

use_ok( 'Rosetta' );
skip( 1, q{is( Rosetta.meta.identifier.version, 0.710.0,
    'Rosetta is the correct version' );} );

use_ok( 'Rosetta::L::en' );
skip( 1, q{is( Rosetta::L::en.meta.identifier.version, 0.200.0,
    'Rosetta::L::en is the correct version' );} );

use_ok( 'Rosetta::Model' );
skip( 1, q{is( Rosetta::Model.meta.identifier.version, 0.710.0,
    'Rosetta::Model is the correct version' );} );

use_ok( 'Rosetta::Model::L::en' );
skip( 1, q{is( Rosetta::Model::L::en.meta.identifier.version, 0.390.0,
    'Rosetta::Model::L::en is the correct version' );} );

use_ok( 'Rosetta::Validator' );
skip( 1, q{is( Rosetta::Validator.meta.identifier.version, 0.710.0,
    'Rosetta::Validator is the correct version' );} );

use_ok( 'Rosetta::Validator::L::en' );
skip( 1, q{is( Rosetta::Validator::L::en.meta.identifier.version, 0.150.0,
    'Rosetta::Validator::L::en is the correct version' );} );
