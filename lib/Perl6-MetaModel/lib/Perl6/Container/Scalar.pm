
package Perl6::Container::Scalar;

use strict;
use warnings;

sub new {
	my $pkg = shift;
	
	my $value;
	my $self = bless {
		value => \$value,
	}, $pkg;

	$self;
}

sub scalar_fetch {
	my $self = shift;

	$self->{value};
}

sub scalar_store {
	my $self = shift;
	my $value = shift;

	$self->{value} = $value;
}

sub scalar_const {
	0;
}

1;
