use 5.008001;
use utf8;
use strict;
use warnings FATAL => 'all';

use Test::More;

plan( 'tests' => 12 );

use_ok( 'QDRDBMS::GSTV' );
is( $QDRDBMS::GSTV::VERSION, 0.000,
    'QDRDBMS::GSTV is the correct version' );

use_ok( 'QDRDBMS::AST' );
is( $QDRDBMS::AST::VERSION, 0.000,
    'QDRDBMS::AST is the correct version' );

use_ok( 'QDRDBMS' );
is( $QDRDBMS::VERSION, 0.000,
    'QDRDBMS is the correct version' );

use_ok( 'QDRDBMS::Validator' );
is( $QDRDBMS::Validator::VERSION, 0.000,
    'QDRDBMS::Validator is the correct version' );

use_ok( 'QDRDBMS::Engine::Example::PhysType' );
is( $QDRDBMS::Engine::Example::PhysType::VERSION, 0.000,
    'QDRDBMS::Engine::Example::PhysType is the correct version' );

use_ok( 'QDRDBMS::Engine::Example' );
is( $QDRDBMS::Engine::Example::VERSION, 0.000,
    'QDRDBMS::Engine::Example is the correct version' );

1; # Magic true value required at end of a reuseable file's code.
