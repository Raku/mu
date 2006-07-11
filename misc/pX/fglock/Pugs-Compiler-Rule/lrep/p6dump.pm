package p6dump;
use strict;
use warnings;
require Exporter;
our @ISA=qw(Exporter);
our @EXPORT=qw(dump_tree); 

use interface;
our $dumper;
#Will work soon...
my %format = (
	yaml => ['YAML',        sub {YAML::Dump($_[0])}],
	p5  =>  ['Data::Dumper',\&p5],
	xml =>  ['XML::Generator',\&xml]
);
sub xmlify {
	my $x    = shift;
	my $val = shift;
	if (ref $val eq 'HASH') {
		return map {
			$x->$_(xmlify($x,$val->{$_}) )
		} keys %{$val};
	} elsif (ref $val eq 'ARRAY') {
		return map {xmlify($x,$_) } @{$val};
	} else {
		return $val;
	}
}
sub xml {
	my $x = new XML::Generator(":pretty");
	return $x->ast(xmlify($x,shift)); 
}
sub p5 {
	local $Data::Dumper::Varname = $_[1];
	$Data::Dumper::Indent = 1;
	Data::Dumper::Dumper($_[0]);
}
sub import {
	$dumper = $format{$interface::format};
	p6dump->export_to_level(1,@_);
	eval "require $dumper->[0]";
	die "install $dumper->[0] to enable $interface::format" if $@;
}
sub dump_tree {
	my $tree = shift;
	my $name = shift;
	return $dumper->[1]($tree,$name);
}
1;
