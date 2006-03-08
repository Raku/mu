package p6dump;
use strict;
use warnings;
require Exporter;
our @ISA=qw(Exporter);
our @EXPORT=qw(dump_tree); 

use interface;
use Data::Dumper;

our $dumper;
#Will work soon...
my %format = (
	yaml => ['YAML',        sub {YAML::Dump($_[0])}],
	p5  =>  ['Data::Dumper',sub {Data::Dumper::Dumper($_[0])}]
);

sub import {
	$dumper = $format{$interface::format};
	print "$interface::format",Dumper($dumper),"\n";
	p6dump->export_to_level(1,@_);
	eval "require $dumper->[0]";
	die "install $dumper->[0] to enable $interface::format" if $@;
}
sub dump_tree {
	my $tree = shift;
	return $dumper->[1]($tree);
}
1;
