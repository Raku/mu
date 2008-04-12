use v6-alpha;

use Test;

plan( 10 );

use_ok( 'Muldis::DB' );
skip( 1, q{is( Muldis::DB.WHO.version, 0.6.2,
    'Muldis::DB is the correct version' );} );

use_ok( 'Muldis::DB::Interface' );
skip( 1, q{is( Muldis::DB::Interface.WHO.version, 0.6.2,
    'Muldis::DB::Interface is the correct version' );} );

use_ok( 'Muldis::DB::Validator' );
skip( 1, q{is( Muldis::DB::Validator.WHO.version, 0.6.2,
    'Muldis::DB::Validator is the correct version' );} );

use_ok( 'Muldis::DB::Engine::Example::Value' );
skip( 1, q{is( Muldis::DB::Engine::Example::Value.WHO.version, 0.0.0,
    'Muldis::DB::Engine::Example::Value is the correct version' );} );

use_ok( 'Muldis::DB::Engine::Example' );
skip( 1, q{is( Muldis::DB::Engine::Example.WHO.version, 0.6.2,
    'Muldis::DB::Engine::Example is the correct version' );} );
