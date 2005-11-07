#!/usr/bin/pugs
use v6;

use lib <t/lib ext/Rosetta-Incubator/t/lib>;

use Test;

plan( 34 );

# Locale-KeyedText:

use_ok( 'Locale::KeyedText' );
skip( q{is( Locale::KeyedText.meta.identifier.version, 1.7.0,
    'Locale::KeyedText is the correct version' );} );

# SQL-Routine:

use_ok( 'SQL::Routine' );
skip( q{is( SQL::Routine.meta.identifier.version, 0.71.0,
    'SQL::Routine is the correct version' );} );

use_ok( 'SQL::Routine::L::en' );
skip( q{is( SQL::Routine::L::en.meta.identifier.version, 0.39.0,
    'SQL::Routine::L::en is the correct version' );} );

# Rosetta:

use_ok( 'Rosetta' );
skip( q{is( Rosetta.meta.identifier.version, 0.49.0,
    'Rosetta is the correct version' );} );

use_ok( 'Rosetta::L::en' );
skip( q{is( Rosetta::L::en.meta.identifier.version, 0.20.0,
    'Rosetta::L::en is the correct version' );} );

use_ok( 'Rosetta::Validator' );
skip( q{is( Rosetta::Validator.meta.identifier.version, 0.49.0,
    'Rosetta::Validator is the correct version' );} );

use_ok( 'Rosetta::Validator::L::en' );
skip( q{is( Rosetta::Validator::L::en.meta.identifier.version, 0.15.0,
    'Rosetta::Validator::L::en is the correct version' );} );

# Rosetta-Engine-Native:

use_ok( 'Rosetta::Engine::Native' );
skip( q{is( Rosetta::Engine::Native.meta.identifier.version, 0.1.0,
    'Rosetta::Engine::Native is the correct version' );} );

use_ok( 'Rosetta::Engine::Native::L::en' );
skip( q{is( Rosetta::Engine::Native::L::en.meta.identifier.version, 0.1.0,
    'Rosetta::Engine::Native::L::en is the correct version' );} );

# SQL-Routine-SQLBuilder:

use_ok( 'SQL::Routine::SQLBuilder' );
skip( q{is( SQL::Routine::SQLBuilder.meta.identifier.version, 0.22.0,
    'SQL::Routine::SQLBuilder is the correct version' );} );

use_ok( 'SQL::Routine::SQLBuilder::L::en' );
skip( q{is( SQL::Routine::SQLBuilder::L::en.meta.identifier.version, 0.3.0,
    'SQL::Routine::SQLBuilder::L::en is the correct version' );} );

# SQL-Routine-SQLParser:

use_ok( 'SQL::Routine::SQLParser' );
skip( q{is( SQL::Routine::SQLParser.meta.identifier.version, 0.3.0,
    'SQL::Routine::SQLParser is the correct version' );} );

use_ok( 'SQL::Routine::SQLParser::L::en' );
skip( q{is( SQL::Routine::SQLParser::L::en.meta.identifier.version, 0.3.0,
    'SQL::Routine::SQLParser::L::en is the correct version' );} );

# Rosetta-Engine-Generic:

use_ok( 'Rosetta::Engine::Generic' );
skip( q{is( Rosetta::Engine::Generic.meta.identifier.version, 0.22.0,
    'Rosetta::Engine::Generic is the correct version' );} );

use_ok( 'Rosetta::Engine::Generic::L::en' );
skip( q{is( Rosetta::Engine::Generic::L::en.meta.identifier.version, 0.14.0,
    'Rosetta::Engine::Generic::L::en is the correct version' );} );

# Rosetta-Emulator-DBI:

use_ok( 'Rosetta::Emulator::DBI' );
skip( q{is( Rosetta::Emulator::DBI.meta.identifier.version, 0.1.0,
    'Rosetta::Emulator::DBI is the correct version' );} );

use_ok( 'Rosetta::Emulator::DBI::L::en' );
skip( q{is( Rosetta::Emulator::DBI::L::en.meta.identifier.version, 0.1.0,
    'Rosetta::Emulator::DBI::L::en is the correct version' );} );

1;
