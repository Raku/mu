#!pugs
use v6;

# More complex dependency trees

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

use Test::More tests => 169;
use Algorithm::Dependency;
use Algorithm::Dependency::Source::File;

# Where is the test data located
my $TESTDATA = 't.data';





# Load the data/complex.txt file in as a source file
my $file = File::Spec.catfile( $TESTDATA, 'complex.txt' );
my $source = Algorithm::Dependency::Source::File.new( $file );
ok( $source, "Complex source created" );
ok( eval {$source.load();}, "Complex source loads" );

# Try it's unordere dependency with nothing selected
my $dep = Algorithm::Dependency.new( source => $source );
ok( $dep, "Algorithm::Dependency.new() returns true" );
ok( ref $dep, "Algorithm::Dependency.new() returns reference" );
isa_ok( $dep, 'Algorithm::Dependency', "Algorithm::Dependency.new() returns correctly" );

# Test each of the dependencies
foreach my $data ( [
	['A'],		[],				['A'] 				], [
	['B'],		['C'],				[qw{B C}] 			], [
	['C'],		[], 				['C']				], [
	['D'],		[qw{E F}],			[qw{D E F}]			], [
	['E'],		['F'],				[qw{E F}]			], [
	['F'],		[],				['F']				], [
	['G'],		[qw{H I J}],			[qw{G H I J}]			], [
	['H'],		[qw{I J}],			[qw{H I J}]			], [
	['I'],		['J'],				[qw{I J}]			], [
	['J'],		[],				['J']				], [
	['K'],		[qw{L M}],			[qw{K L M}]			], [
	['L'],		['M'],				[qw{L M}]			], [
	['M'],		[],				['M']				], [
	['N'],		[],				['N']				], [
	['O'],		['N'],				[qw{N O}]			], [
	['P'],		['N'],				[qw{N P}]			], [
	['Q'],		[qw{N O}],			[qw{N O Q}]			], [
	['R'],		[qw{N P}],			[qw{N P R}]			], [
	['S'],		[qw{N O P Q R}],		[qw{N O P Q R S}]		], [
	['T'],		[qw{A D E F K L M N P R}],	[qw{A D E F K L M N P R T}]	]
) {
	my $args = @{ $data.[0] }.map:{ "'$_'" }.join( ', ' );
	my $rv = $dep.depends( @{ $data.[0] } );
	ok( $rv, "Dependency.depends($args) returns something" );
	is_deeply( $rv, $data.[1], "Dependency.depends($args) returns expected values" );
	$rv = $dep.schedule( @{ $data.[0] } );
	ok( $rv, "Dependency.schedule($args) returns something" );
	is_deeply( $rv, $data.[2], "Dependency.schedule($args) returns expected values" );
}





# Try an unordered dependency with half a dozen random things selected
$dep = Algorithm::Dependency.new( source => $source, selected => [qw{F H J N R P}] );
ok( $dep, "Algorithm::Dependency.new() returns true" );
ok( ref $dep, "Algorithm::Dependency.new() returns reference" );
isa_ok( $dep, 'Algorithm::Dependency', "Algorithm::Dependency.new() returns correctly" );

# Test each of the dependencies
foreach my $data ( [
	['A'],		[],			['A'] 			], [
	['B'],		['C'],			[qw{B C}] 		], [
	['C'],		[], 			['C']			], [
	['D'],		['E'],			[qw{D E}]		], [
	['E'],		[],			['E']			], [
	['F'],		[],			[]			], [
	['G'],		['I'],			[qw{G I}]		], [
	['H'],		['I'],			['I']			], [
	['I'],		[],			['I']			], [
	['J'],		[],			[]			], [
	['K'],		[qw{L M}],		[qw{K L M}]		], [
	['L'],		['M'],			[qw{L M}]		], [
	['M'],		[],			['M']			], [
	['N'],		[],			[]			], [
	['O'],		[],			['O']			], [
	['P'],		[],			[]			], [
	['Q'],		['O'],			[qw{O Q}]		], [
	['R'],		[],			[]			], [
	['S'],		[qw{O Q}],		[qw{O Q S}]		], [
	['T'],		[qw{A D E K L M}], 	[qw{A D E K L M T}]	]
) {
	my $args = @{ $data.[0] }.map:{ "'$_'" }.join( ', ' );
	my $rv = $dep.depends( @{ $data.[0] } );
	ok( $rv, "Dependency.depends($args) returns something" );
	is_deeply( $rv, $data.[1], "Dependency.depends($args) returns expected values" );
	$rv = $dep.schedule( @{ $data.[0] } );
	ok( $rv, "Dependency.schedule($args) returns something" );
	is_deeply( $rv, $data.[2], "Dependency.schedule($args) returns expected values" );
}

# Do a quick check of the missing_dependencies methods
is( $source.missing_dependencies, 0, ".missing_dependencies() returns as expected" );

1;
