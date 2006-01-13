#!/usr/bin/pugs
use v6;

use lib <t/lib misc/Rosetta-Incubator/t/lib>;

use Test;

plan( 16 );

# Rosetta-Utility-SQLBuilder:

use_ok( 'Rosetta::Utility::SQLBuilder' );
skip( 1, q{is( Rosetta::Utility::SQLBuilder.meta.identifier.version, 0.230.0,
    'Rosetta::Utility::SQLBuilder is the correct version' );} );

use_ok( 'Rosetta::Utility::SQLBuilder::L::en' );
skip( 1, q{is( Rosetta::Utility::SQLBuilder::L::en.meta.identifier.version, 0.40.0,
    'Rosetta::Utility::SQLBuilder::L::en is the correct version' );} );

# Rosetta-Utility-SQLParser:

use_ok( 'Rosetta::Utility::SQLParser' );
skip( 1, q{is( Rosetta::Utility::SQLParser.meta.identifier.version, 0.40.0,
    'Rosetta::Utility::SQLParser is the correct version' );} );

use_ok( 'Rosetta::Utility::SQLParser::L::en' );
skip( 1, q{is( Rosetta::Utility::SQLParser::L::en.meta.identifier.version, 0.40.0,
    'Rosetta::Utility::SQLParser::L::en is the correct version' );} );

# Rosetta-Engine-Generic:

use_ok( 'Rosetta::Engine::Generic' );
skip( 1, q{is( Rosetta::Engine::Generic.meta.identifier.version, 0.230.0,
    'Rosetta::Engine::Generic is the correct version' );} );

use_ok( 'Rosetta::Engine::Generic::L::en' );
skip( 1, q{is( Rosetta::Engine::Generic::L::en.meta.identifier.version, 0.150.0,
    'Rosetta::Engine::Generic::L::en is the correct version' );} );

# Rosetta-Emulator-DBI:

use_ok( 'Rosetta::Emulator::DBI' );
skip( 1, q{is( Rosetta::Emulator::DBI.meta.identifier.version, 0.1.0,
    'Rosetta::Emulator::DBI is the correct version' );} );

use_ok( 'Rosetta::Emulator::DBI::L::en' );
skip( 1, q{is( Rosetta::Emulator::DBI::L::en.meta.identifier.version, 0.1.0,
    'Rosetta::Emulator::DBI::L::en is the correct version' );} );
