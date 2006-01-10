#!/usr/bin/pugs
use v6;

use lib <t/lib ext/Rosetta-Engine-Native/t/lib>;

use Test;

plan( 4 );

use_ok( 'Rosetta::Engine::Native' );
skip( 1, q{is( Rosetta::Engine::Native.meta.identifier.version, 0.1.0,
    'Rosetta::Engine::Native is the correct version' );} );

use_ok( 'Rosetta::Engine::Native::L::en' );
skip( 1, q{is( Rosetta::Engine::Native::L::en.meta.identifier.version, 0.1.0,
    'Rosetta::Engine::Native::L::en is the correct version' );} );
