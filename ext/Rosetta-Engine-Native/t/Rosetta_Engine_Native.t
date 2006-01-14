#!/usr/bin/pugs
use v6;

use lib <t/lib ext/Rosetta-Engine-Native/t/lib>;

use Test;

plan( 4 );

use Rosetta::Engine::Native; pass "(dummy instead of broken use_ok)";
skip( 1, q{is( Rosetta::Engine::Native.meta.identifier.version, 0.1.0,
    'Rosetta::Engine::Native is the correct version' );} );

use Rosetta::Engine::Native::L::en; pass "(dummy instead of broken use_ok)";
skip( 1, q{is( Rosetta::Engine::Native::L::en.meta.identifier.version, 0.1.0,
    'Rosetta::Engine::Native::L::en is the correct version' );} );
