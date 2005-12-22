#!/usr/bin/pugs
use v6;

use lib <t/lib ext/Rosetta-Incubator/t/lib>;

use Test;

plan( 34 );

# Locale-KeyedText:

use_ok( 'Locale::KeyedText' );
skip( 1, q{is( Locale::KeyedText.meta.identifier.version, 1.72.0,
    'Locale::KeyedText is the correct version' );} );

# SQL-Routine:

use_ok( 'SQL::Routine' );
skip( 1, q{is( SQL::Routine.meta.identifier.version, 0.710.0,
    'SQL::Routine is the correct version' );} );

use_ok( 'SQL::Routine::L::en' );
skip( 1, q{is( SQL::Routine::L::en.meta.identifier.version, 0.390.0,
    'SQL::Routine::L::en is the correct version' );} );

# Rosetta:

use_ok( 'Rosetta' );
skip( 1, q{is( Rosetta.meta.identifier.version, 0.490.0,
    'Rosetta is the correct version' );} );

use_ok( 'Rosetta::L::en' );
skip( 1, q{is( Rosetta::L::en.meta.identifier.version, 0.200.0,
    'Rosetta::L::en is the correct version' );} );

use_ok( 'Rosetta::Validator' );
skip( 1, q{is( Rosetta::Validator.meta.identifier.version, 0.490.0,
    'Rosetta::Validator is the correct version' );} );

use_ok( 'Rosetta::Validator::L::en' );
skip( 1, q{is( Rosetta::Validator::L::en.meta.identifier.version, 0.150.0,
    'Rosetta::Validator::L::en is the correct version' );} );

# Rosetta-Engine-Native:

use_ok( 'Rosetta::Engine::Native' );
skip( 1, q{is( Rosetta::Engine::Native.meta.identifier.version, 0.1.0,
    'Rosetta::Engine::Native is the correct version' );} );

use_ok( 'Rosetta::Engine::Native::L::en' );
skip( 1, q{is( Rosetta::Engine::Native::L::en.meta.identifier.version, 0.1.0,
    'Rosetta::Engine::Native::L::en is the correct version' );} );

# SQL-Routine-SQLBuilder:

use_ok( 'SQL::Routine::SQLBuilder' );
skip( 1, q{is( SQL::Routine::SQLBuilder.meta.identifier.version, 0.220.0,
    'SQL::Routine::SQLBuilder is the correct version' );} );

use_ok( 'SQL::Routine::SQLBuilder::L::en' );
skip( 1, q{is( SQL::Routine::SQLBuilder::L::en.meta.identifier.version, 0.30.0,
    'SQL::Routine::SQLBuilder::L::en is the correct version' );} );

# SQL-Routine-SQLParser:

use_ok( 'SQL::Routine::SQLParser' );
skip( 1, q{is( SQL::Routine::SQLParser.meta.identifier.version, 0.30.0,
    'SQL::Routine::SQLParser is the correct version' );} );

use_ok( 'SQL::Routine::SQLParser::L::en' );
skip( 1, q{is( SQL::Routine::SQLParser::L::en.meta.identifier.version, 0.30.0,
    'SQL::Routine::SQLParser::L::en is the correct version' );} );

# Rosetta-Engine-Generic:

use_ok( 'Rosetta::Engine::Generic' );
skip( 1, q{is( Rosetta::Engine::Generic.meta.identifier.version, 0.220.0,
    'Rosetta::Engine::Generic is the correct version' );} );

use_ok( 'Rosetta::Engine::Generic::L::en' );
skip( 1, q{is( Rosetta::Engine::Generic::L::en.meta.identifier.version, 0.140.0,
    'Rosetta::Engine::Generic::L::en is the correct version' );} );

# Rosetta-Emulator-DBI:

use_ok( 'Rosetta::Emulator::DBI' );
skip( 1, q{is( Rosetta::Emulator::DBI.meta.identifier.version, 0.1.0,
    'Rosetta::Emulator::DBI is the correct version' );} );

use_ok( 'Rosetta::Emulator::DBI::L::en' );
skip( 1, q{is( Rosetta::Emulator::DBI::L::en.meta.identifier.version, 0.1.0,
    'Rosetta::Emulator::DBI::L::en is the correct version' );} );
