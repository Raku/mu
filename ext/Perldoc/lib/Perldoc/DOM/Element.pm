

package Perldoc::DOM::Element;

use strict;
use warnings;
use base 'Perldoc::DOM::Node';

=head1 NAME

Perldoc::DOM::Element - element in a Perldoc::DOM tree

=head1 SYNOPSIS

See L<Perldoc::DOM::Node>.

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
    super($o);
}

1;
