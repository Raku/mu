use v6-alpha;

use lib <t/lib ext/Rosetta/t/lib>;

use Test;

plan( 20 );

use Rosetta; pass "(dummy instead of broken use_ok)";
skip( 1, q{is( Rosetta.WHO.version, 0.724.1,
    'Rosetta is the correct version' );} );

use Rosetta::L::en; pass "(dummy instead of broken use_ok)";
skip( 1, q{is( Rosetta::L::en.WHO.version, 0.210.2,
    'Rosetta::L::en is the correct version' );} );

use Rosetta::Model; pass "(dummy instead of broken use_ok)";
skip( 1, q{is( Rosetta::Model.WHO.version, 0.724.1,
    'Rosetta::Model is the correct version' );} );

use Rosetta::Model::L::en; pass "(dummy instead of broken use_ok)";
skip( 1, q{is( Rosetta::Model::L::en.WHO.version, 0.400.2,
    'Rosetta::Model::L::en is the correct version' );} );

use Rosetta::Validator; pass "(dummy instead of broken use_ok)";
skip( 1, q{is( Rosetta::Validator.WHO.version, 0.724.1,
    'Rosetta::Validator is the correct version' );} );

use Rosetta::Validator::L::en; pass "(dummy instead of broken use_ok)";
skip( 1, q{is( Rosetta::Validator::L::en.WHO.version, 0.160.2,
    'Rosetta::Validator::L::en is the correct version' );} );

use Rosetta::Engine::Example; pass "(dummy instead of broken use_ok)";
skip( 1, q{is( Rosetta::Engine::Example.WHO.version, 0.724.1,
    'Rosetta::Engine::Example is the correct version' );} );

use Rosetta::Engine::Example::L::en; pass "(dummy instead of broken use_ok)";
skip( 1, q{is( Rosetta::Engine::Example::L::en.WHO.version, 0.2.2,
    'Rosetta::Engine::Example::L::en is the correct version' );} );

use Rosetta::Shell; pass "(dummy instead of broken use_ok)";
skip( 1, q{is( Rosetta::Shell.WHO.version, 0.1.3,
    'Rosetta::Shell is the correct version' );} );

use Rosetta::Shell::L::en; pass "(dummy instead of broken use_ok)";
skip( 1, q{is( Rosetta::Shell::L::en.WHO.version, 0.1.2,
    'Rosetta::Shell::L::en is the correct version' );} );
