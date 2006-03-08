package p6dump;
use strict;
use warnings;
require Exporter;
our @ISA=qw(Exporter);
our @EXPORT=qw(dump_tree); 

use interface;
use Data::Dumper ();
our $use_yaml = $interface::yaml;

#Will work soon...
my %formats = {
	yaml => ['YAML',        sub {YAML::Dump($_[0])}],
	p5  =>  ['Data::Dumper',sub {Data::Dumper::Dumper($_[0])}]
};

sub import {
	p6dump->export_to_level(1,@_);
	eval {require YAML};
	$use_yaml = 0 if $@;
	die "install YAML::Dumper to enable --yaml" if $@ and $interface::yaml;
}
sub dump_tree {
	my $tree = shift;
	return YAML::Dump($tree) if $use_yaml;
	return Data::Dumper::Dumper($tree);
}
1;
