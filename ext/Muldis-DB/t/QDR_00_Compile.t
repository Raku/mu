use v6-alpha;

use Test;

plan( 12 );

use_ok( 'QDRDBMS::AST' );
skip( 1, q{is( QDRDBMS::AST.WHO.version, 0.0.0,
    'QDRDBMS::AST is the correct version' );} );

use_ok( 'QDRDBMS' );
skip( 1, q{is( QDRDBMS.WHO.version, 0.0.0,
    'QDRDBMS is the correct version' );} );

use_ok( 'QDRDBMS::Validator' );
skip( 1, q{is( QDRDBMS::Validator.WHO.version, 0.0.0,
    'QDRDBMS::Validator is the correct version' );} );

use_ok( 'QDRDBMS::Engine::Example::PhysType' );
skip( 1, q{is( QDRDBMS::Engine::Example::PhysType.WHO.version, 0.0.0,
    'QDRDBMS::Engine::Example::PhysType is the correct version' );} );

use_ok( 'QDRDBMS::Engine::Example::Operators' );
skip( 1, q{is( QDRDBMS::Engine::Example::Operators.WHO.version, 0.0.0,
    'QDRDBMS::Engine::Example::Operators is the correct version' );} );

use_ok( 'QDRDBMS::Engine::Example' );
skip( 1, q{is( QDRDBMS::Engine::Example.WHO.version, 0.0.0,
    'QDRDBMS::Engine::Example is the correct version' );} );
