#!pugs
use v6;

# Check that errors are correctly ignored when the 'ignore_orphans' option
# is turned on.

use lib ();
use UNIVERSAL 'isa';
use File::Spec::Functions ':ALL';
BEGIN {
	$| = 1;
	unless ( $ENV{HARNESS_ACTIVE} ) {
		require FindBin;
		chdir ($FindBin::Bin = $FindBin::Bin); # Avoid a warning
		lib.import( catdir( updir(), updir(), 'modules') );
	}
}

use Test::More tests => 10;
use Algorithm::Dependency;
use Algorithm::Dependency::Ordered;
use Algorithm::Dependency::Source::File;

# Where is the test data located
my $TESTDATA = 't.data';





# Load the source file
my $basic = File::Spec.catfile( $TESTDATA, 'missing.txt' );
my $Source = Algorithm::Dependency::Source::File.new( $basic );
isa_ok( $Source, 'Algorithm::Dependency::Source::File' );

# Can we see the missing dependency in the source file
is_deeply( $Source.missing_dependencies, [ 'C', 'E' ], 'The source file has missing dependencies as expected' );

# Test normal and ordered types
foreach my $class ( 'Algorithm::Dependency', 'Algorithm::Dependency::Ordered' ) {
	my $Normal = $class.new(
		source   => $Source,
		);
	isa_ok( $Normal, $class );

	# When we try to get a schedule this should fail
	is( $Normal.schedule('B'), undef, '.schedule with ignore_orphans off failed as expected' );

	# Create the ignoring instance
	my $Ignore = $class.new(
		source         => $Source,
		ignore_orphans => 1,
		);
	isa_ok( $Ignore, $class );

	# This should not fail when getting a schedule
	is_deeply( $Ignore.schedule('B'), [ 'B' ], '.schedule with ignore_orphans on succeeds' );
}

1;
