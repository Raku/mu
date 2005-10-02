#!/usr/bin/pugs
use v6;

use Test;

plan( 30 );

# Locale-KeyedText:

use_ok( 'Locale::KeyedText' );
is( Locale::KeyedText.meta.identifier.version, 1.7.0,
    'Locale::KeyedText is the correct version' );

# SQL-Routine:

use_ok( 'SQL::Routine' );
is( SQL::Routine.meta.identifier.version, 0.71.0,
    'SQL::Routine is the correct version' );

use_ok( 'SQL::Routine::L::en' );
is( SQL::Routine::L::en.meta.identifier.version, 0.39.0,
    'SQL::Routine::L::en is the correct version' );

# Rosetta:

use_ok( 'Rosetta' );
is( Rosetta.meta.identifier.version, 0.49.0,
    'Rosetta is the correct version' );

use_ok( 'Rosetta::L::en' );
is( Rosetta::L::en.meta.identifier.version, 0.20.0,
    'Rosetta::L::en is the correct version' );

use_ok( 'Rosetta::Validator' );
is( Rosetta::Validator.meta.identifier.version, 0.49.0,
    'Rosetta::Validator is the correct version' );

use_ok( 'Rosetta::Validator::L::en' );
is( Rosetta::Validator::L::en.meta.identifier.version, 0.15.0,
    'Rosetta::Validator::L::en is the correct version' );

# SQL-Routine-SQLBuilder:

use_ok( 'SQL::Routine::SQLBuilder' );
is( SQL::Routine::SQLBuilder.meta.identifier.version, 0.22.0,
    'SQL::Routine::SQLBuilder is the correct version' );

use_ok( 'SQL::Routine::SQLBuilder::L::en' );
is( SQL::Routine::SQLBuilder::L::en.meta.identifier.version, 0.3.0,
    'SQL::Routine::SQLBuilder::L::en is the correct version' );

# SQL-Routine-SQLParser:

use_ok( 'SQL::Routine::SQLParser' );
is( SQL::Routine::SQLParser.meta.identifier.version, 0.3.0,
    'SQL::Routine::SQLParser is the correct version' );

use_ok( 'SQL::Routine::SQLParser::L::en' );
is( SQL::Routine::SQLParser::L::en.meta.identifier.version, 0.3.0,
    'SQL::Routine::SQLParser::L::en is the correct version' );

# Rosetta-Engine-Generic:

use_ok( 'Rosetta::Engine::Generic' );
is( Rosetta::Engine::Generic.meta.identifier.version, 0.22.0,
    'Rosetta::Engine::Generic is the correct version' );

use_ok( 'Rosetta::Engine::Generic::L::en' );
is( Rosetta::Engine::Generic::L::en.meta.identifier.version, 0.14.0,
    'Rosetta::Engine::Generic::L::en is the correct version' );

# Rosetta-Emulator-DBI:

use_ok( 'Rosetta::Emulator::DBI' );
is( Rosetta::Emulator::DBI.meta.identifier.version, 0.1.0,
    'Rosetta::Emulator::DBI is the correct version' );

use_ok( 'Rosetta::Emulator::DBI::L::en' );
is( Rosetta::Emulator::DBI::L::en.meta.identifier.version, 0.1.0,
    'Rosetta::Emulator::DBI::L::en is the correct version' );

1;
