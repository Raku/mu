#!pugs
use v6;

# Creating and using dependency trees

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

use Test::More tests => 117;
use Algorithm::Dependency;
use Algorithm::Dependency::Source::File;

# Where is the test data located
my $TESTDATA = 't.data';





# Load the data/basics.txt file in as a source file, and test it rigorously.
my $file = File::Spec.catfile( $TESTDATA, 'basics.txt' );
my $source = Algorithm::Dependency::Source::File.new( $file );

ok( $source, "Source is true" );
ok( ref $source, "Source is a reference" );
isa_ok( $source, 'Algorithm::Dependency::Source::File', "Source is a Source::File" );
isa_ok( $source, 'Algorithm::Dependency::Source', "Source is a Source" );
ok( exists $source.loaded, "Source has a loaded value" );
ok( ! $source.loaded, "Source isn't loaded" );

ok( eval {$source.load();}, "Source .load() returns true" );
ok( $source.loaded, "Source appears to be loaded" );
isa_ok( $source.item('A'), 'Algorithm::Dependency::Item', ".item() returns an Item for A" );
isa_ok( $source.item('B'), 'Algorithm::Dependency::Item', ".item() returns an Item for B" );
isa_ok( $source.item('D'), 'Algorithm::Dependency::Item', ".item() returns an Item for D" );
ok( ! defined $source.item('BAD'), ".item() for bad value properly returns undef" );

ok( $source.item('A').id() eq 'A', "Item .id() appears to work ok" );
ok( scalar $source.item('A').depends() == 0, "Item .depends() for 0 depends returns ok" );
ok( scalar $source.item('B').depends() == 1, "Item .depends() for 1 depends returns ok" );
ok( scalar $source.item('D').depends() == 2, "Item .depends() for 2 depends returns ok" );

my @items = $source.items();
ok( scalar @items == 6, "Source .items() returns a list" );
isa_ok( $items[0], 'Algorithm::Dependency::Item', "List contains Items" );
isa_ok( $items[1], 'Algorithm::Dependency::Item', "List contains Items" );
isa_ok( $items[3], 'Algorithm::Dependency::Item', "List contains Items" );
ok( ($items[0].id() eq 'A' and $items[1].id() eq 'B' and $items[3].id() eq 'D'), "Source .items() returns in original database order" );
ok( $items[0] eq $source.item('A'), "Hash and list refer to the same object" );





# Try to create a basic unordered dependency
my $dep = Algorithm::Dependency.new( source => $source );
ok( $dep, "Algorithm::Dependency.new() returns true" );
ok( ref $dep, "Algorithm::Dependency.new() returns reference" );
isa_ok( $dep, 'Algorithm::Dependency', "Algorithm::Dependency.new() returns correctly" );
ok( $dep.source, "Dependency.source() returns true" );
ok( $dep.source() eq $source, "Dependency.source() returns the original source" );
ok( $dep.item('A'), "Dependency.item() returns true" );
ok( $dep.item('A') eq $source.item('A'), "Dependency.item() returns the same as Source.item" );
my @tmp;
ok( scalar( @tmp = $dep.selected_list() ) == 0, "Dependency.selected_list() returns empty list" );
ok( ! $dep.selected('Foo'), "Dependency.selected() returns false on bad input" );
ok( ! $dep.selected('A'), "Dependency.selected() returns false when not selected" );
ok( ! defined $dep.depends('Foo'), "Dependency.depends() fails correctly on bad input" );
foreach my $data ( [
	['A'],		[],		['A'] 			], [
	['B'],		['C'],		['B','C'] 		], [
	['C'],		[], 		['C']			], [
	['D'],		['E','F'],	[qw{D E F}]		], [
	['E'],		[],		['E']			], [
	['F'],		[],		['F']			], [
	['A','B'],	['C'],		[qw{A B C}]		], [
	['B','D'],	[qw{C E F}],	[qw{B C D E F}]		]
) {
	my $args = @{ $data.[0] }.map:{ "'$_'" }.join( ', ' );
	my $rv = $dep.depends( @{ $data.[0] } );
	ok( $rv, "Dependency.depends($args) returns something" );
	is_deeply( $rv, $data.[1], "Dependency.depends($args) returns expected values" );
	$rv = $dep.schedule( @{ $data.[0] } );
	ok( $rv, "Dependency.schedule($args) returns something" );
	is_deeply( $rv, $data.[2], "Dependency.schedule($args) returns expected values" );
}

# Try a bad creation
ok( ! defined Algorithm::Dependency.new(), "Dependency.new() fails correctly" );

# Create with one selected
$dep = Algorithm::Dependency.new( source => $source, selected => [ 'F' ] );
ok( $dep, "Algorithm::Dependency.new() returns true" );
ok( ref $dep, "Algorithm::Dependency.new() returns reference" );
isa_ok( $dep, 'Algorithm::Dependency', "Algorithm::Dependency.new() returns correctly" );
ok( $dep.source, "Dependency.source() returns true" );
ok( $dep.source() eq $source, "Dependency.source() returns the original source" );
ok( $dep.item('A'), "Dependency.item() returns true" );
ok( $dep.item('A') eq $source.item('A'), "Dependency.item() returns the same as Source.item" );
ok( scalar( @tmp = $dep.selected_list() ) == 1, "Dependency.selected_list() returns empty list" );
ok( ! $dep.selected('Foo'), "Dependency.selected() returns false when wrong" );
ok( ! $dep.selected('A'), "Dependency.selected() returns false when expected" );
ok( $dep.selected('F'), "Dependency.selected() return true" );
ok( ! defined $dep.depends('Foo'), "Dependency.depends() fails correctly on bad input" );
foreach my $data ( [
	['A'],		[],		['A'] 			], [
	['B'],		['C'],		[qw{B C}] 		], [
	['C'],		[], 		['C']		], [
	['D'],		['E'],		[qw{D E}]	], [
	['E'],		[],		['E']		], [
	['F'],		[],		[]		], [
	['A','B'],	['C'],		[qw{A B C}]	], [
	['B','D'],	[qw{C E}],	[qw{B C D E}]	]
) {
	my $args = @{ $data.[0] }.map:{ "'$_'" }.join( ', ' );
	my $rv = $dep.depends( @{ $data.[0] } );
	ok( $rv, "Dependency.depends($args) returns something" );
	is_deeply( $rv, $data.[1], "Dependency.depends($args) returns expected values" );
	$rv = $dep.schedule( @{ $data.[0] } );
	ok( $rv, "Dependency.schedule($args) returns something" );
	is_deeply( $rv, $data.[2], "Dependency.schedule($args) returns expected values" );
}

# Does missing dependencies return defined but false for a source we
# know doesn't have any missing dependencies
is( $source.missing_dependencies, 0, ".missing_dependencies() returns as expected when nothing missing" );

# Load the source we know has missing dependencies
$file = File::Spec.catfile( $TESTDATA, 'missing.txt' );
my $Missing = Algorithm::Dependency::Source::File.new( $file );
ok( $Missing, "Missing is true" );
ok( ref $Missing, "Missing is a reference" );
isa_ok( $Missing, 'Algorithm::Dependency::Source::File', "Missing is a Source::File" );
isa_ok( $Missing, 'Algorithm::Dependency::Source', "Missing is a Source" );
ok( eval {$Missing.load();}, "Missing .load() returns true" );

is_deeply( $Missing.missing_dependencies, [ 'C', 'E' ], ".missing_dependencies() returns as expected when something missing" );

1;

