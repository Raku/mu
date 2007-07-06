use v6-alpha;

use Test;

plan( 12 );

use_ok( 'Muldis::DB::AST' );
skip( 1, q{is( Muldis::DB::AST.WHO.version, 0.1.0,
    'Muldis::DB::AST is the correct version' );} );

use_ok( 'Muldis::DB' );
skip( 1, q{is( Muldis::DB.WHO.version, 0.1.0,
    'Muldis::DB is the correct version' );} );

use_ok( 'Muldis::DB::Validator' );
skip( 1, q{is( Muldis::DB::Validator.WHO.version, 0.1.0,
    'Muldis::DB::Validator is the correct version' );} );

use_ok( 'Muldis::DB::Engine::Example::PhysType' );
skip( 1, q{is( Muldis::DB::Engine::Example::PhysType.WHO.version, 0.1.0,
    'Muldis::DB::Engine::Example::PhysType is the correct version' );} );

use_ok( 'Muldis::DB::Engine::Example::Operators' );
skip( 1, q{is( Muldis::DB::Engine::Example::Operators.WHO.version, 0.1.0,
    'Muldis::DB::Engine::Example::Operators is the correct version' );} );

use_ok( 'Muldis::DB::Engine::Example' );
skip( 1, q{is( Muldis::DB::Engine::Example.WHO.version, 0.1.0,
    'Muldis::DB::Engine::Example is the correct version' );} );
