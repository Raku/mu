use v6-alpha;

use Test;

plan( 10 );

use QDRDBMS::AST; pass "(dummy instead of broken use_ok)";
skip( 1, q{is( QDRDBMS::AST.WHO.version, 0.0.0,
    'QDRDBMS::AST is the correct version' );} );

use QDRDBMS; pass "(dummy instead of broken use_ok)";
skip( 1, q{is( QDRDBMS.WHO.version, 0.0.0,
    'QDRDBMS is the correct version' );} );

use QDRDBMS::Validator; pass "(dummy instead of broken use_ok)";
skip( 1, q{is( QDRDBMS::Validator.WHO.version, 0.0.0,
    'QDRDBMS::Validator is the correct version' );} );

use QDRDBMS::Engine::Example::PhysType; pass "(dummy instead of broken use_ok)";
skip( 1, q{is( QDRDBMS::Engine::Example::PhysType.WHO.version, 0.0.0,
    'QDRDBMS::Engine::Example::PhysType is the correct version' );} );

use QDRDBMS::Engine::Example; pass "(dummy instead of broken use_ok)";
skip( 1, q{is( QDRDBMS::Engine::Example.WHO.version, 0.0.0,
    'QDRDBMS::Engine::Example is the correct version' );} );
