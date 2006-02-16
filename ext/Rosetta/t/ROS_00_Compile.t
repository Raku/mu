#!/usr/bin/pugs
use v6;

use lib <t/lib ext/Rosetta/t/lib>;

use Test;

plan( 16 );

use Rosetta; pass "(dummy instead of broken use_ok)";
skip( 1, q{is( Rosetta.meta.identifier.version, 0.721.0,
    'Rosetta is the correct version' );} );

use Rosetta::L::en; pass "(dummy instead of broken use_ok)";
skip( 1, q{is( Rosetta::L::en.meta.identifier.version, 0.210.0,
    'Rosetta::L::en is the correct version' );} );

use Rosetta::Model; pass "(dummy instead of broken use_ok)";
skip( 1, q{is( Rosetta::Model.meta.identifier.version, 0.721.0,
    'Rosetta::Model is the correct version' );} );

use Rosetta::Model::L::en; pass "(dummy instead of broken use_ok)";
skip( 1, q{is( Rosetta::Model::L::en.meta.identifier.version, 0.400.0,
    'Rosetta::Model::L::en is the correct version' );} );

use Rosetta::Validator; pass "(dummy instead of broken use_ok)";
skip( 1, q{is( Rosetta::Validator.meta.identifier.version, 0.721.0,
    'Rosetta::Validator is the correct version' );} );

use Rosetta::Validator::L::en; pass "(dummy instead of broken use_ok)";
skip( 1, q{is( Rosetta::Validator::L::en.meta.identifier.version, 0.160.0,
    'Rosetta::Validator::L::en is the correct version' );} );

use Rosetta::Engine::Example; pass "(dummy instead of broken use_ok)";
skip( 1, q{is( Rosetta::Engine::Example.meta.identifier.version, 0.721.0,
    'Rosetta::Engine::Example is the correct version' );} );

use Rosetta::Engine::Example::L::en; pass "(dummy instead of broken use_ok)";
skip( 1, q{is( Rosetta::Engine::Example::L::en.meta.identifier.version, 0.2.0,
    'Rosetta::Engine::Example::L::en is the correct version' );} );
