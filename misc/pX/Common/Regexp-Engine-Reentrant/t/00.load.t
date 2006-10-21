use Test::More tests => 3;

BEGIN {
use_ok( 'Regexp::Engine::Reentrant' );
use_ok( 'Regexp::Engine::Reentrant::Match' );
use_ok( 'Regexp::Engine::Reentrant::Backtrack' );
}

diag( "Testing Regexp::Engine::Reentrant $Regexp::Engine::Reentrant::VERSION" );
