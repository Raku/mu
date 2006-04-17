package Pugs::Utils::Dump;
use strict;
use warnings;

require Exporter;
our @ISA=qw(Exporter);
our @EXPORT=qw(dump_tree); 

use Pugs::Utils::Interface;
my $dumper;

# which modules do diffrent formats use
my %format = (
	yaml => ['YAML'           => \&yaml],
	p5  =>  ['Data::Dumper'   => \&p5  ],
	xml =>  ['XML::Generator' => \&xml ],
);

# dump data as yaml
sub yaml {
	YAML::Dump($_[0]);
}

# dump data as xml
sub xml {
	my $x = new XML::Generator(":pretty");
	return $x->ast(xmlify($x,$_[0])); 
}

# xml helper routine
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

# dump data as p5
sub p5 {
	local $Data::Dumper::Varname = $_[1];
	$Data::Dumper::Indent = 1;
	Data::Dumper::Dumper($_[0]);
}

sub import {
	# get the format specified on the command line
	$dumper = $format{$Pugs::Util::Interface::format};

	# load the required module
	eval "require $dumper->[0]";
	die "install $dumper->[0] to enable $interface::format" if $@;

	#export the funtions
	Pugs::Util::Dump->export_to_level(1,@_);
}

sub dump_tree {
	my ($tree,$name) = @_;
	return $dumper->[1]($tree,$name);
}
1;
