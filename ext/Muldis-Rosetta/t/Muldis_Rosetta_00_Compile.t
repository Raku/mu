use v6-alpha;

use Test;

plan( 8 );

use_ok( 'Muldis::Rosetta' );
skip( 1, q{is( Muldis::Rosetta.WHO.version, 0.7.0,
    'Muldis::Rosetta is the correct version' );} );

use_ok( 'Muldis::Rosetta::Interface' );
skip( 1, q{is( Muldis::Rosetta::Interface.WHO.version, 0.7.0,
    'Muldis::Rosetta::Interface is the correct version' );} );

use_ok( 'Muldis::Rosetta::Validator' );
skip( 1, q{is( Muldis::Rosetta::Validator.WHO.version, 0.7.0,
    'Muldis::Rosetta::Validator is the correct version' );} );

use_ok( 'Muldis::Rosetta::Engine::Example' );
skip( 1, q{is( Muldis::Rosetta::Engine::Example.WHO.version, 0.7.0,
    'Muldis::Rosetta::Engine::Example is the correct version' );} );
