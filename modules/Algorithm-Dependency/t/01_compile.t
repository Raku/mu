#!pugs
use v6;

# Basic load and method existance tests for Algorithm::Dependency

use lib ();
use File::Spec::Functions ':ALL';
BEGIN {
	$| = 1;
	unless ( $ENV{HARNESS_ACTIVE} ) {
		require FindBin;
		chdir ($FindBin::Bin = $FindBin::Bin); # Avoid a warning
		lib.import( catdir( updir(), updir(), 'modules') );
	}
}

use Test::More tests => 4;




# Check their perl version
ok( 1, 'Perl version is new enough' );

# Load the main modules
use_ok( 'Algorithm::Dependency' );
use_ok( 'Algorithm::Dependency::Ordered' );
use_ok( 'Algorithm::Dependency::Source::File' );
