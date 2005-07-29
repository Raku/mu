
package Perl6::Container::Scalar;

use strict;
use warnings;

sub new {
	my $class = shift;
	return bless { value => \(my $value) }, $class;
}

sub CONST { 0 }

sub FETCH { (shift)->{value} }

sub STORE {
	my ($self, $value) = @_;
	$self->{value} = $value;
	$self;
}

1;
