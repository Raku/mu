

package Kwid::DOM::Element;

use strict;
use warnings;
use base 'Kwid::DOM::Node';

=head1 NAME

Kwid::DOM::Element - element in a Kwid::DOM tree

=head1 SYNOPSIS

See L<Kwid::DOM::Node>.

=head1 DESCRIPTION

An `element'.

All meta-information provided by dialects should be stored in this
tree as an attribute.  The property C<attr> is used for this.

=cut

# is there a Spiffy field way to do this?
sub attr {
    my $self = shift;
    if ( defined(my $attr = shift) ) {
	if ( ref $attr eq "HASH" ) {
	    $self->{attr} = $attr;
	} else {
	    if ( my $value = shift ) {
		$self->{attr}{$attr} = $value;
	    } else {
		return $self->{attr}{$attr};
	    }
	}
    } else {
	return $self->{attr};
    }
}

sub _init {
    my $self = shift;
    my $o = shift;

    $self->attr($o->{attr}) if $self->{attr};
}

1;
